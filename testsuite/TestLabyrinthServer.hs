{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinthServer where

import Control.Monad

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as AE
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as BS
import Data.List
import qualified Data.Map as M
import Data.String (IsString, fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import System.Directory

import Yesod hiding ((.=))

import Test.Framework
import Test.Framework.TestTypes (Test)

import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Test

import Labyrinth
import LabyrinthServer
import LabyrinthServer.JSON

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

requestPost :: String -> [(String, String)] -> Session SResponse
requestPost path post = srequest $ SRequest req $ postString post
    where req = setPath defaultPost $ fromString path
          defaultPost = defaultRequest { requestMethod = fromString "POST"
                                       , requestHeaders = formHeader }
          formHeader = [(hContentType, fromString formContentType)]
          formContentType = "application/x-www-form-urlencoded; charset=UTF-8"
          postString = fromString
                     . intercalate "&"
                     . map (\(k, v) -> k ++ "=" ++ v)

jsonBody :: FromJSON a => SResponse -> Maybe a
jsonBody = decode . simpleBody

(.=) :: ToJSON a => String -> a -> Pair
k .= v = (AE..=) (fromString k) v

assertJsonBody :: SResponse -> Value -> Session ()
assertJsonBody resp val = assertBody (encode val) resp

encodeString :: ToJSON a => a -> String
encodeString = T.unpack . E.decodeUtf8 . BS.toStrict . encode

test_interaction = runLabyrinthSession $ do
    gameList <- requestGet "games"
    assertBody (encode $ object []) gameList
    newGame <- requestPost "game" [ ("players", "2")
                                  , ("width",   "5")
                                  , ("height",  "5")
                                  ]
    assertBody (encode "ok") newGame
    gameList' <- requestGet "games"
    let (Just games) = jsonBody gameList' :: Maybe (M.Map String Value)
    liftIO $ assertEqual 1 $ length $ M.keys games
    let [gameId] = M.keys games
    let gameUrl = "game/" ++ gameId
    gameLog <- requestGet gameUrl
    assertJsonBody gameLog $
        object [ "log" .= ([] :: [()])
               , "game" .= object [ "currentTurn" .= (0 :: Int)
                                  , "height" .= (5 :: Int)
                                  , "width" .= (5 :: Int)
                                  , "playerCount" .= (2 :: Int)
                                  , "gameEnded" .= False
                                  , "positionsChosen" .= False
                                  ]
               ]
    let gameMoveUrl = gameUrl ++ "/move"
    move1 <- requestPost gameMoveUrl [ ("player", "0")
                                     , ("move", "say hello")
                                     ]
    assertJsonBody move1 $ object [ "string" .= "ok"
                                  , "results" .= ([] :: [()])
                                  ]
    move2 <- requestPost gameMoveUrl [ ("player", "0")
                                     , ("move", "choose 1 1")
                                     ]
    assertJsonBody move2 $ object [ "string" .= "position chosen"
                                  , "results" .= [
                                        object [ "type" .= "choose position"
                                               , "result" .= "position chosen"
                                               ]
                                    ]
                                  ]
    -- This will start the game with unpredictable exact result, not checking
    requestPost gameMoveUrl [ ("player", "1")
                            , ("move", "choose 1 1")
                            ]
    move3 <- requestPost gameMoveUrl [ ("player", fromString "0")
                                     , ("move", encodeString $ Sensitive True $ Move [Grenade R])
                                     ]
    assertJsonBody move3 $ object [ "string" .= "grenade thrown"
                                  , "results" .= [
                                        object [ "type" .= "grenade"
                                               , "result" .= "grenade thrown"
                                               ]
                                    ]
                                  ]
