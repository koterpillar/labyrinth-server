{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinthServer where

import Control.Monad

import Data.Aeson
import Data.String (fromString)

import System.Directory

import Yesod

import Test.Framework
import Test.Framework.TestTypes (Test)

import Network.Wai.Test

import LabyrinthServer

{-# ANN module "HLint: ignore Use camelCase" #-}

testDataPath = "test_state"

makeTestServer :: (LabyrinthServer -> IO a) -> IO a
makeTestServer cont = do
    testDataExists <- doesDirectoryExist testDataPath
    when testDataExists $ removeDirectoryRecursive testDataPath
    makeServer testDataPath cont

runLabyrinthSession :: Session a -> IO a
runLabyrinthSession s = makeTestServer $ \server -> do
    app <- toWaiApp server
    runSession s app

requestGet :: String -> Session SResponse
requestGet = request . setPath defaultRequest . fromString

test_game_list = runLabyrinthSession $ do
    gameList <- requestGet "games"
    assertBody (encode $ object []) gameList
