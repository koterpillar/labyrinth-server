{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# Language TemplateHaskell #-}

module TestLabyrinthServer.JSON where

import Control.Applicative
import Control.Monad

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.DeriveTH

import Labyrinth

import LabyrinthServer.JSON

import Test.Framework

derive makeArbitrary ''Direction
derive makeArbitrary ''MoveDirection

simpleAction = oneof [ Go <$> arbitrary
                     , Shoot <$> arbitrary
                     , Grenade <$> arbitrary
                     , return Surrender
                     ]

instance Arbitrary Action where
    arbitrary = oneof [ simpleAction
                      , Conditional
                            <$> return "condition"
                            <*> listOf simpleAction
                            <*> listOf simpleAction
                      ]

derive makeArbitrary ''QueryType
derive makeArbitrary ''Position

instance Arbitrary Move where
    arbitrary = oneof [ liftM Move arbitrary
                      , liftM ChoosePosition arbitrary
                      , liftM ReorderCell arbitrary
                      , liftM Query $ listOf1 arbitrary
                      , liftM Say arbitrary
                      ]

derive makeArbitrary ''CellTypeResult
derive makeArbitrary ''CellEvents
derive makeArbitrary ''TreasureResult
derive makeArbitrary ''GoResult
derive makeArbitrary ''ShootResult
derive makeArbitrary ''GrenadeResult
derive makeArbitrary ''ActionResult
derive makeArbitrary ''ChoosePositionResult
derive makeArbitrary ''ReorderCellResult
derive makeArbitrary ''Health
derive makeArbitrary ''QueryResult
derive makeArbitrary ''StartResult
derive makeArbitrary ''MoveResult

isSecret :: Move -> Bool
isSecret (ChoosePosition _) = True
isSecret (ReorderCell _)    = True
isSecret _                  = False

prop_move_to_json :: Move -> Bool
prop_move_to_json = (0 <) . BS.length . encode . toSensitiveJSON True

-- prop_show_parse_move :: Move -> Bool
-- prop_show_parse_move
