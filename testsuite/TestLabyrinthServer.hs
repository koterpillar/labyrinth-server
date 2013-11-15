{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module TestLabyrinthServer where

import Control.Monad

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

test_game_list = runLabyrinthSession $ do
    gameList <- request $ setPath defaultRequest "/games"
    assertBody "{}" gameList
