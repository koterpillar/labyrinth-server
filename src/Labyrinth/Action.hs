module Labyrinth.Action (performMove) where

import Control.Monad.State

import Data.List
import Data.Maybe

import Labyrinth.Map
import Labyrinth.Move

import Peeker

performMove :: PlayerId -> Move -> State Labyrinth MoveResult
performMove pi (Move actions) = do
    current <- getS currentPlayer
    if current /= pi
        then return WrongTurn
        else if length (filter isMovement actions) > 1
            then return InvalidMove
            else do
                actionRes <- performActions actions
                queue <- alivePlayers
                pCount <- gets playerCount
                let next = head $ tail $ dropWhile (current /=) $ cycle queue
                updS currentPlayer next
                return $ MoveRes actionRes

isMovement :: Action -> Bool
isMovement (Go _) = True
isMovement _ = False

transferAmmo :: Maybe Int -> Peek Labyrinth Int -> Peek Labyrinth Int -> State Labyrinth Int
transferAmmo maxAmount from to = do
    found <- getS from
    has <- getS to
    let amount = case maxAmount of
                     (Just max) -> min found $ max - has
                     Nothing    -> found
    let found' = found - amount
    let has' = has + amount
    updS from found'
    updS to has'
    return found

transferAmmo_ :: Maybe Int -> Peek Labyrinth Int -> Peek Labyrinth Int -> State Labyrinth ()
transferAmmo_ maxAmount from to = do
    transferAmmo maxAmount from to
    return ()

afterMove :: CellType -> Position -> PlayerId -> State Labyrinth (Maybe Position)
afterMove Land _ _ = return Nothing
afterMove Armory _ pi = do
    updS (player pi ~> pbullets) maxBullets
    updS (player pi ~> pgrenades) maxGrenades
    return Nothing
afterMove Hospital _ pi = do
    updS (player pi ~> phealth) Healthy
    return Nothing
afterMove (Pit i) _ pi = do
    npits <- gets pitCount
    let i' = (i + 1) `mod` npits
    npos <- gets (pit i')
    return $ Just npos
afterMove (River d) npos pi = do
    let npos' = advance npos d
    return $ Just npos'
afterMove RiverDelta _ _ = return Nothing

returnContinue :: [Action] -> ActionResult -> State Labyrinth [ActionResult]
returnContinue rest res = do
    restRes <- performActions rest
    return $ res:restRes

returnStop :: ActionResult -> State Labyrinth [ActionResult]
returnStop = return . (:[])

alwaysContinue :: [Action] -> State Labyrinth ActionResult -> State Labyrinth [ActionResult]
alwaysContinue rest act = do
    res <- act
    returnContinue rest res

performActions :: [Action] -> State Labyrinth [ActionResult]
performActions [] = return []

performActions (Go (Towards dir):rest) = let returnCont = returnContinue rest in do
    pi <- getS currentPlayer
    pos <- getS (player pi ~> position)
    w <- getS (wall pos dir)
    if w == NoWall
        then do
            let npos = advance pos dir
            updS (player pi ~> position) npos
            out <- gets $ isOutside npos
            if out
                then do
                    t <- getS (player pi ~> ptreasure)
                    case t of
                        Nothing -> returnCont $ GoR $ WentOutside Nothing
                        (Just FakeTreasure) -> do
                            updS (player pi ~> ptreasure) Nothing
                            returnCont $ GoR $ WentOutside $ Just TurnedToAshesR
                        (Just TrueTreasure) -> do
                            updS (player pi ~> ptreasure) Nothing
                            returnStop $ GoR $ WentOutside $ Just TrueTreasureR
                            -- TODO: mark the game as ended?
                else do
                    ct <- getS (cell npos ~> ctype)
                    -- Perform cell-type-specific actions
                    npos' <- afterMove ct npos pi
                    let npos'' = fromMaybe npos npos'
                    updS (player pi ~> position) npos''
                    -- If transported, determine the new cell type
                    nct <- if isJust npos' then do
                            nct' <- getS (cell npos'' ~> ctype)
                            return $ Just nct'
                        else
                            return Nothing
                    -- Pick ammo
                    cb <- transferAmmo (Just maxBullets)
                        (cell npos'' ~> cbullets)
                        (player pi ~> pbullets)
                    cg <- transferAmmo (Just maxGrenades)
                        (cell npos'' ~> cgrenades)
                        (player pi ~> pgrenades)
                    -- Pick treasures
                    ctr <- getS (cell npos'' ~> ctreasures)
                    ptr <- getS (player pi ~> ptreasure)
                    if and [ptr == Nothing, length ctr > 0]
                        then do
                            let ctr' = tail ctr
                            let ptr' = Just $ head ctr
                            updS (cell npos'' ~> ctreasures) ctr'
                            updS (player pi ~> ptreasure) ptr'
                        else
                            return ()
                    let nctr = (fmap ctResult) nct
                    returnCont $ GoR $ Went (ctResult ct) cb cg (length ctr) nctr
        else
            returnCont $ GoR HitWall

performActions (Grenade dir:rest) = alwaysContinue rest $ do
    pi <- getS currentPlayer
    g <- getS (player pi ~> pgrenades)
    if g > 0
        then do
            updS (player pi ~> pgrenades) (g - 1)
            pos <- getS (player pi ~> position)
            out <- gets $ isOutside pos
            if out then return ()
                else do
                    w <- getS (wall pos dir)
                    if w /= HardWall
                        then do
                            updS (wall pos dir) NoWall
                        else
                            return ()
            return $ GrenadeR GrenadeOK
        else
            return $ GrenadeR NoGrenades

performActions (Shoot dir:rest) = alwaysContinue rest $ do
    pi <- getS currentPlayer
    b <- getS (player pi ~> pbullets)
    if b > 0
        then do
            pos <- getS (player pi ~> position)
            ct <- getS (cell pos ~> ctype)
            if ct == Hospital || ct == Armory
                then return $ ShootR Forbidden
                else do
                    updS (player pi ~> pbullets) (b - 1)
                    res <- performShoot pos dir
                    return $ ShootR res
        else
            return $ ShootR NoBullets

alivePlayers :: State Labyrinth [PlayerId]
alivePlayers = do
    cnt <- gets playerCount
    filterM playerAlive [0..cnt - 1]
    where playerAlive :: PlayerId -> State Labyrinth Bool
          playerAlive i = do
              ph <- getS (player i ~> phealth)
              return $ ph /= Dead

playersAliveAt :: Position -> State Labyrinth [PlayerId]
playersAliveAt pos = do
    alive <- alivePlayers
    filterM (playerAt pos) alive

playerAt :: Position -> PlayerId -> State Labyrinth Bool
playerAt pos i = do
    pp <- getS (player i ~> position)
    return $ pos == pp

performShoot :: Position -> Direction -> State Labyrinth ShootResult
performShoot pos dir = do
    outside <- gets $ isOutside pos
    ct <- getS (cell pos ~> ctype)
    pi <- getS currentPlayer
    cnt <- gets playerCount
    hit <- playersAliveAt pos
    if not outside && ct == Hospital
        then return ShootOK
        else do
            let othersHit = delete pi hit
            if length othersHit == 0
                then if outside
                    then return ShootOK
                    else do
                        if ct == Armory
                            then return ShootOK
                            else do
                                w <- getS (wall pos dir)
                                if w == NoWall
                                    then performShoot (advance pos dir) dir
                                    else return ShootOK
                else do
                    forM_ othersHit $ \i -> do
                        ph <- getS (player i ~> phealth)
                        if outside
                            then updS (player i ~> pbullets) 0
                            else transferAmmo_ Nothing (player i ~> pbullets) (cell pos ~> cbullets)
                        when (ph == Healthy) $ do
                            updS (player i ~> phealth) Wounded
                        when (ph == Wounded) $ do
                            if outside
                                then updS (player i ~> pgrenades) 0
                                else transferAmmo_ Nothing (player i ~> pgrenades) (cell pos ~> cgrenades)
                            updS (player i ~> phealth) Dead
                    return Scream
