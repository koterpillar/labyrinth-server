{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module LabyrinthServer.Data where

import Control.Lens hiding (Action, (.=))
import Control.Monad.State
import Control.Monad.Reader (ask)

import Data.Acid (Query, Update, makeAcidic)
import Data.Aeson.Types (Pair)
import Data.DeriveTH
import Data.Derive.Typeable
import qualified Data.Map as M
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Vector as V

import System.Random

import Yesod hiding (get, Update)

import Labyrinth hiding (performMove)
import qualified Labyrinth as L

deriveSafeCopy 0 'base ''Direction
deriveSafeCopy 0 'base ''Wall
deriveSafeCopy 0 'base ''CellType
deriveSafeCopy 0 'base ''Cell
deriveSafeCopy 0 'base ''Position
deriveSafeCopy 0 'base ''Treasure
deriveSafeCopy 0 'base ''Health
deriveSafeCopy 0 'base ''Player
deriveSafeCopy 0 'base ''Labyrinth

deriveSafeCopy 0 'base ''Action
deriveSafeCopy 0 'base ''MoveDirection
deriveSafeCopy 0 'base ''QueryType
deriveSafeCopy 0 'base ''Move

deriveSafeCopy 0 'base ''CellTypeResult
deriveSafeCopy 0 'base ''TreasureResult
deriveSafeCopy 0 'base ''CellEvents
deriveSafeCopy 0 'base ''GoResult
deriveSafeCopy 0 'base ''GrenadeResult
deriveSafeCopy 0 'base ''ShootResult
deriveSafeCopy 0 'base ''ActionResult
deriveSafeCopy 0 'base ''ChoosePositionResult
deriveSafeCopy 0 'base ''ReorderCellResult
deriveSafeCopy 0 'base ''QueryResult
deriveSafeCopy 0 'base ''StartResult
deriveSafeCopy 0 'base ''MoveResult

derive makeTypeable ''Labyrinth
derive makeTypeable ''Move
derive makeTypeable ''MoveResult

type GameId = String

data MoveRecord = MoveRecord { _rplayer :: PlayerId
                             , _rmove :: Move
                             , _rresult :: MoveResult
                             , _rstate :: Labyrinth
                             }

makeLenses ''MoveRecord

deriveSafeCopy 0 'base ''MoveRecord

derive makeTypeable ''MoveRecord

type MoveLog = [MoveRecord]

logMoveResult :: MoveRecord -> State MoveLog ()
logMoveResult m = modify (++ [m])

data Game = Game { _labyrinth :: Labyrinth
                 , _moves :: MoveLog
                 }

newGame :: Labyrinth -> Game
newGame l = Game l []

makeLenses ''Game

deriveSafeCopy 0 'base ''Game

derive makeTypeable ''Game

data Games = Games { _games :: M.Map GameId Game }

noGames :: Games
noGames = Games M.empty

makeLenses ''Games

game :: GameId -> Simple Traversal Games Game
game gid = games . ix gid

getGames :: Query Games Games
getGames = ask

stateUpdate :: State x y -> Update x y
stateUpdate f = do
    st <- get
    let (r, st') = runState f st
    put st'
    return r

data LabyrinthParams = LabyrinthParams { lpwidth   :: Int
                                       , lpheight  :: Int
                                       , lpplayers :: Int
                                       }

createLabyrinth :: (MonadIO m) => LabyrinthParams -> m Labyrinth
createLabyrinth p = do
    gen <- liftIO getStdGen
    let (l, gen') = generateLabyrinth
                        (lpwidth p) (lpheight p) (lpplayers p) gen
    liftIO $ setStdGen gen'
    return l

addGame :: GameId -> Labyrinth -> Update Games Bool
addGame gid lab = stateUpdate $ zoom games $ do
    existing <- gets (M.member gid)
    if existing
        then return False
        else do
            modify $ M.insert gid $ newGame lab
            return True

getGame :: GameId -> Query Games Game
getGame = view . singular . game

performMove :: GameId -> PlayerId -> Move -> Update Games MoveResult
performMove g p m = stateUpdate $ zoom (singular $ game g) $ do
    r <- zoom labyrinth $ L.performMove p m
    l <- use labyrinth
    zoom moves $ logMoveResult $ MoveRecord p m r l
    return r

removeGame :: GameId -> Update Games ()
removeGame gid = stateUpdate $ zoom games $ do
    modify $ M.delete gid
    return ()

deriveSafeCopy 0 'base ''Games

derive makeTypeable ''Games

makeAcidic ''Games [ 'getGames
                   , 'addGame
                   , 'getGame
                   , 'performMove
                   , 'removeGame
                   ]

exampleMoves :: [Move]
exampleMoves = [ ChoosePosition (Pos 2 4)
               , Move [goTowards L]
               , Move [Shoot U]
               , Move [Grenade D, goTowards D]
               , ReorderCell (Pos 3 3)
               , Query [BulletCount, GrenadeCount, PlayerHealth]
               , Say "hello"
               , Move [Conditional "hit a wall" [Grenade D] [Shoot L]]
               , Move [Surrender]
               ]

exampleMovesJSON :: Value
exampleMovesJSON = array $ map show exampleMoves

class ToSensitiveJSON a where
    toSensitiveJSON :: Bool -> a -> Value

instance ToSensitiveJSON a => ToSensitiveJSON [a] where
    toSensitiveJSON s = Array . V.fromList . map (toSensitiveJSON s)

data Sensitive a = Sensitive { isSensitive :: Bool, sensitiveData :: a }
instance ToSensitiveJSON a => ToJSON (Sensitive a) where
    toJSON (Sensitive s a) = toSensitiveJSON s a

instance ToJSON Direction where
    toJSON d = toJSON $ show d

instance ToJSON CellType where
    toJSON ct = object $ [ "type" .= show ct ] ++ prop ct
                    where prop (Pit n)   = ["number" .= n]
                          prop (River d) = ["direction" .= d]
                          prop _         = []

instance ToJSON Treasure where
    toJSON t = toJSON $ show t

instance ToJSON Cell where
    toJSON c = object [ "cell"      .= (c ^. ctype)
                      , "bullets"   .= (c ^. cbullets)
                      , "grenades"  .= (c ^. cgrenades)
                      , "treasures" .= (c ^. ctreasures)
                      ]

instance ToJSON Position where
    toJSON p = object [ "x" .= pX p
                      , "y" .= pY p
                      ]

instance ToJSON Wall where
    toJSON NoWall   = String "none"
    toJSON Wall     = String "wall"
    toJSON HardWall = String "hardwall"

mapToList :: M.Map Position v -> [[v]]
mapToList m = [[(M.!) m (Pos x y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
    where xmin = minimum xs
          xmax = maximum xs
          ymin = minimum ys
          ymax = maximum ys
          xs = map pX ps
          ys = map pY ps
          ps = M.keys m

instance ToSensitiveJSON Labyrinth where
    toSensitiveJSON s l = object $ [ "width"           .= (l ^. labWidth)
                                   , "height"          .= (l ^. labHeight)
                                   , "currentTurn"     .= (l ^. currentTurn)
                                   , "gameEnded"       .= (l ^. gameEnded)
                                   , "positionsChosen" .= (l ^. positionsChosen)
                                   , "playerCount"     .= playerCount l
                                   ] ++ sensitive
               where sensitive | s = [ "map"    .= show l
                                     , "cells"  .= mapToList (l ^. cells)
                                     , "wallsW" .= mapToList (l ^. wallsV)
                                     , "wallsH" .= mapToList (l ^. wallsH)
                                     ]
                               | otherwise = []

instance ToSensitiveJSON Move where
    toSensitiveJSON s m = object $ [ "string" .= show m
                                   ] ++ moveData s m

moveData :: Bool -> Move -> [Pair]
moveData s m = []

instance ToSensitiveJSON MoveResult where
    toSensitiveJSON _ m = object [ "string" .= show m
                                 ]

instance ToSensitiveJSON MoveRecord where
    toSensitiveJSON s r = object [ "player" .= (r ^. rplayer)
                                 , "move"   .= Sensitive s (r ^. rmove)
                                 , "result" .= Sensitive s (r ^. rresult)
                                 , "state"  .= Sensitive s (r ^. rstate)
                                 ]

instance ToJSON Game where
    toJSON g = object [ "game" .= Sensitive ended (g ^. labyrinth)
                      , "log"  .= Sensitive ended (g ^. moves)
                      ]
                  where ended = g ^. labyrinth ^. gameEnded

instance ToJSON Games where
    toJSON g = object [T.pack id .= game | (id, game) <- lst]
        where lst = M.toList $ g ^. games
