{-# LANGUAGE OverloadedStrings     #-}
module LabyrinthServer.JSON where

import Control.Applicative
import Control.Lens hiding (Action, (.=))
import Control.Monad

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Text.Parsec as P

import Labyrinth hiding (performMove)
import qualified Labyrinth as L
import qualified Labyrinth.Read as LR

import LabyrinthServer.Data

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
    toJSON ct = object $ withType (show ct) $ prop ct
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
    toSensitiveJSON s m = object $ ("string" .= show m) : moveData s m

withType :: String -> [Pair] -> [Pair]
withType t = (("type" .= t):)

moveData :: Bool -> Move -> [Pair]
moveData s (Move as) = withType "move" ["actions" .= as]
moveData s (ChoosePosition p) = withType "choose_position"
    ["position" .= p | s]
moveData s (ReorderCell p) = withType "reorder_cell" ["position" .= p | s]
moveData s (Query qs) = withType "query" ["queries" .= qs]
moveData s (Say text) = withType "say" ["text" .= text]

instance ToJSON Action where
    toJSON (Go Next) = object $ withType "go"
        ["direction" .= ("next" :: String)]
    toJSON (Go (Towards dir)) = object $ withType "go" ["direction" .= dir]
    toJSON (Shoot dir) = object $ withType "shoot" ["direction" .= dir]
    toJSON (Grenade dir) = object $ withType "grenade" ["direction" .= dir]
    toJSON Surrender = object $ withType "surrender" []
    toJSON (Conditional cif cthen celse) = object $ withType "conditional"
        [ "if" .= cif
        , "then" .= cthen
        , "else" .= celse
        ]

instance ToJSON QueryType where
    toJSON BulletCount = "bullets"
    toJSON GrenadeCount = "grenades"
    toJSON PlayerHealth = "health"
    toJSON TreasureCarried = "treasure"

instance ToJSON MoveResult where
    toJSON m@(MoveRes as) = object [ "string" .= show m
                                   , "results" .= as
                                   ]

objWithType :: ToJSON a => String -> a -> [Pair]
objWithType s o = withType s $ HM.toList m
    where Object m = toJSON o

instance ToJSON ActionResult where
    toJSON ar = object $ toPairs ar

toPairs :: ActionResult -> [Pair]
toPairs (GoR r) =             objWithType "go" r
toPairs shot@(ShootR r) =     withType "shoot" [ "result" .= show shot]
toPairs gr@(GrenadeR r) =     withType "grenade" [ "result" .= show gr]
toPairs Surrendered  =        withType "surrendered" []
toPairs (WoundedAlert pi h) = withType "wounded" [ "player" .= pi
                                                 , "health" .= h
                                                 ]
toPairs (ChoosePositionR r) = withType "choose position" ["result" .= show r]
toPairs (ReorderCellR r)    = objWithType "reorder cell" r
toPairs (QueryR r)          = objWithType "query" r
toPairs (GameStarted rs)    = withType "game started" ["results" .= rs]
toPairs Draw                = withType "draw" []
toPairs WrongTurn           = withType "wrong turn" []
toPairs InvalidMove         = withType "invalid move" []

instance ToJSON GoResult where
    toJSON (Went ct cr) =     object [ "result" .= ("success" :: String)
                                     , "cell"   .= ct
                                     , "events" .= cr
                                     ]
    toJSON (WentOutside tr) = object [ "result"          .= ("outside" :: String)
                                     , "treasure_result" .= tr
                                     ]
    toJSON (HitWall cr) =     object [ "result" .= ("wall" :: String)
                                     , "events" .= cr
                                     ]
    toJSON LostOutside =      object [ "result" .= ("lost outside" :: String)
                                     ]
    toJSON InvalidMovement =  object [ "result" .= ("invalid movement" :: String)
                                     ]

instance ToJSON ReorderCellResult where
    toJSON (ReorderOK ct cr) = object [ "result" .= ("success" :: String)
                                      , "cell"   .= ct
                                      , "events" .= cr
                                      ]
    toJSON ReorderForbidden  = object [ "result" .= ("forbidden" :: String)
                                      ]

instance ToJSON CellTypeResult where
    toJSON = toJSON . show

instance ToJSON CellEvents where
    toJSON ev = object $ [ "found_bullets" .= (ev ^. foundBullets)
                         , "found_grenades" .= (ev ^. foundGrenades)
                         , "found_treasures" .= (ev ^. foundTreasures)
                         ] ++ transported
        where transported = case ev ^. transportedTo of
                  Nothing -> []
                  Just ct -> ["transported_to" .= ct]

instance ToJSON TreasureResult where
    toJSON TurnedToAshesR = "turned to ashes"
    toJSON TrueTreasureR  = "victory"

instance ToJSON QueryResult where
    toJSON (BulletCountR b)     = object [ "query" .= ("bullets" :: String)
                                         , "count" .= b
                                         ]
    toJSON (GrenadeCountR g)    = object [ "query" .= ("grenades" :: String)
                                         , "value" .= g
                                         ]
    toJSON (HealthR h)          = object [ "query" .= ("health" :: String)
                                         , "value" .= h
                                         ]
    toJSON (TreasureCarriedR t) = object [ "query" .= ("treasure" :: String)
                                         , "value" .= t
                                         ]

instance ToJSON Health where
    toJSON = toJSON . show

instance ToJSON StartResult where
    toJSON r = object [ "player" .= (r ^. splayer)
                      , "cell"   .= (r ^. scell)
                      , "events" .= (r ^. sevents)
                      ]

instance ToSensitiveJSON MoveRecord where
    toSensitiveJSON s r = object [ "player" .= (r ^. rplayer)
                                 , "move"   .= Sensitive s (r ^. rmove)
                                 , "result" .= (r ^. rresult)
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

withObject' = withObject "object expected"

withObjectType :: (String -> Object -> Parser a) -> Value -> Parser a
withObjectType f = withObject' $ \o -> do
    t <- o .: "type"
    f t o

stringParser :: P.Parsec String () a -> Value -> Parser a
stringParser parser (String str) = case P.parse parser "" (T.unpack str) of
        Left err -> error $ show err
        Right res -> return res
stringParser _ _ = error "string expected"

instance FromJSON Move where
    parseJSON = withObjectType $ \t o -> case t of
        "move" -> Move <$> o .: "actions"
        "choose_position" -> ChoosePosition <$> o .: "position"
        "reorder_cell" -> ReorderCell <$> o .: "position"
        "query" -> Query <$> o .: "queries"
        "say" -> Say <$> o .: "text"

instance FromJSON Action where
    parseJSON = withObjectType $ \t o -> case t of
        "go" -> Go <$> o .: "direction"
        "shoot" -> Shoot <$> o .: "direction"
        "grenade" -> Grenade <$> o .: "direction"
        "surrender" -> pure Surrender
        "conditional" -> Conditional <$> o .: "if"
                                     <*> o .: "then"
                                     <*> o .: "else"

instance FromJSON Direction where
    parseJSON = stringParser LR.direction

instance FromJSON MoveDirection where
    parseJSON (String "next") = return Next
    parseJSON x = liftM Towards $ parseJSON x

instance FromJSON Position where
    parseJSON = withObject' $ \o -> Pos <$> o .: "x" <*> o .: "y"

instance FromJSON QueryType where
    parseJSON = stringParser LR.queryParser
