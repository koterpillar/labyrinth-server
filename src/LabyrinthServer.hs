{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module LabyrinthServer where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket, fromException, handleJust)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class

import Data.Acid ( AcidState
                 , EventResult
                 , EventState
                 , openLocalStateFrom
                 , QueryEvent
                 , UpdateEvent
                 )
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BS
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.String as S

import Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import System.Environment
import System.FilePath.Posix
import System.Random

import Text.Hamlet (hamletFile)
import Text.Julius (juliusFile)
import Text.Lucius (luciusFile)

import Yesod hiding (delete, update)
import Yesod.Static

import Labyrinth hiding (performMove)

import LabyrinthServer.Data
import LabyrinthServer.JSON

newId :: (MonadIO m) => m String
newId = replicateM 32 $ liftIO $ randomRIO ('a', 'z')

envVar :: String -> IO (Maybe String)
envVar var = do
    env <- liftM M.fromList getEnvironment
    return $ M.lookup var env

envVarWithDefault :: String -> String -> IO String
envVarWithDefault def var =
    liftM (fromMaybe def) (envVar var)

getDataPath :: IO String
getDataPath = do
    dataDir <- envVarWithDefault "." "OPENSHIFT_DATA_DIR"
    return $ dataDir </> "state"

data WatchTarget = GameList | GameLog GameId
                   deriving (Eq, Ord)

data ConnectionInfo = ConnectionInfo { ciId   :: String
                                     , ciConn :: WS.Connection
                                     }
instance Eq ConnectionInfo where
    (==) = (==) `on` ciId

mkConnInfo :: WS.Connection -> IO ConnectionInfo
mkConnInfo conn = do
    id <- newId
    return $ ConnectionInfo id conn

type Watchers = M.Map WatchTarget [ConnectionInfo]

data LabyrinthServer = LabyrinthServer { lsGames    :: AcidState Games
                                       , lsStatic   :: Static
                                       , lsWatchers :: MVar Watchers
                                       }

staticFiles "static"

mkYesod "LabyrinthServer" [parseRoutes|
/                    HomeR          GET
/games               GamesR         GET
/game                NewGameR       POST
/game/#GameId        GameR          GET
/game/#GameId/move   MakeMoveR      POST
/game/#GameId/delete DeleteGameR    DELETE
/examples            ExampleMovesR  GET
/static              StaticR        Static lsStatic
|]

instance Yesod LabyrinthServer where
    defaultLayout = mainLayout

makeServer :: FilePath -> (LabyrinthServer -> IO a) -> IO a
makeServer dataPath cont = do
    static <- static "static"
    bracket
        (openLocalStateFrom dataPath noGames)
        createCheckpointAndClose $
        \acid -> do
            watchers <- newMVar M.empty
            cont $ LabyrinthServer acid static watchers

labyrinthMain :: IO ()
labyrinthMain = do
    dataPath <- getDataPath
    makeServer dataPath $ \server -> do
        port <- liftM read $ envVarWithDefault "8080" "PORT"
        ip <- envVarWithDefault "127.0.0.1" "OPENSHIFT_INTERNAL_IP"
        app <- toWaiApp server
        let intercept = WaiWS.intercept $ wsHandler server
        let settings = defaultSettings { settingsPort      = port
                                       , settingsHost      = Host ip
                                       , settingsIntercept = intercept
                                       }
        runSettings settings app

instance RenderMessage LabyrinthServer FormMessage where
    renderMessage _ _ = defaultFormMessage

wsHandler :: LabyrinthServer -> WS.PendingConnection -> IO ()
wsHandler site rq = do
    let path = T.unpack $ E.decodeUtf8 $ WS.requestPath $ WS.pendingRequest rq
    -- TODO: parse path better
    let watch = if path == "/games" then GameList else GameLog $ drop 6 path
    conn <- WS.acceptRequest rq
    ci <- mkConnInfo conn
    addWatcher site watch ci
    forever $ handleConnClosed (removeWatcher site watch ci) $ do
        -- Ignore all received messages
        WS.receive conn
        return ()

handleConnClosed :: IO a -> IO a -> IO a
handleConnClosed handler = handleJust whenClosed (const handler)
    where whenClosed e = case fromException e :: Maybe WS.ConnectionException of
                             Just ce -> Just ce
                             _ -> Nothing

withWatchers :: LabyrinthServer -> (Watchers -> IO Watchers) -> IO ()
withWatchers site = modifyMVar_ $ lsWatchers site

addWatcher :: LabyrinthServer -> WatchTarget -> ConnectionInfo -> IO ()
addWatcher site watch ci = withWatchers site $ \watchers ->
        return $ M.insertWith (++) watch [ci] watchers

removeWatcher :: LabyrinthServer -> WatchTarget -> ConnectionInfo -> IO ()
removeWatcher site watch ci = withWatchers site $ \watchers ->
        return $ removeWatcher' watch ci watchers

removeWatcher' :: WatchTarget -> ConnectionInfo -> Watchers -> Watchers
removeWatcher' watch ci = M.update (nullMaybe . delete ci) watch
    where nullMaybe [] = Nothing
          nullMaybe x  = Just x

query :: (QueryEvent event, EventState event ~ Games)
      => event
      -> Handler (EventResult event)
query ev = do
    site <- getYesod
    let acid = lsGames site
    query' acid ev

update :: (UpdateEvent event, EventState event ~ Games)
       => WatchTarget
       -> event
       -> Handler (EventResult event)
update watch ev = do
    site <- getYesod
    let acid = lsGames site
    res <- update' acid ev
    notifyWatchers site watch
    return res

notifyWatchers :: (MonadIO m) => LabyrinthServer -> WatchTarget -> m ()
notifyWatchers site watch = liftIO $ withWatchers site $ \watchersMap -> do
    let watchers = fromMaybe [] $ M.lookup watch watchersMap
    value <- watchTargetValue site watch
    let notifyOne watchersMap ci = handleConnClosed
            (return $ removeWatcher' watch ci watchersMap)
            $ do
                WS.sendTextData (ciConn ci) (encode value)
                return watchersMap
    foldM notifyOne watchersMap watchers

watchTargetValue :: (MonadIO m) => LabyrinthServer -> WatchTarget -> m Value
watchTargetValue site GameList = do
    let acid = lsGames site
    result <- query' acid GetGames
    return $ toJSON result
watchTargetValue site (GameLog gameId) = do
    let acid = lsGames site
    result <- query' acid $ GetGame gameId
    return $ toJSON result

immediateResponse :: WatchTarget -> Handler Value
immediateResponse target = do
    site <- getYesod
    result <- watchTargetValue site target
    returnCORSJson result

postForm :: (Html -> MForm Handler (FormResult a, Widget))
         -> (a -> Handler Value)
         -> Handler Value
postForm form handler = do
    ((result, _), _) <- runFormPostNoToken form
    case result of
        FormSuccess value -> handler value
        FormFailure errors -> returnCORSJson errors

addCORSHeader :: MonadHandler m => m ()
addCORSHeader = addHeader "Access-Control-Allow-Origin" "*"

returnCORSJson :: (MonadHandler m, ToJSON a) => a -> m Value
returnCORSJson v = do
    addCORSHeader
    returnJson v

mainLayout :: Widget -> Handler Html
mainLayout widget = do
    p <- widgetToPageContent widget
    giveUrlRenderer $(hamletFile "templates/layout.hamlet")

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/handlebars.js/1.0.0/handlebars.min.js"
    toWidget $(juliusFile "templates/index.julius")
    toWidget $(luciusFile "templates/index.lucius")
    $(whamletFile "templates/index.hamlet")

getGamesR :: Handler Value
getGamesR = immediateResponse GameList

named :: T.Text -> FieldSettings LabyrinthServer
named name = FieldSettings "" Nothing Nothing (Just name) []

newGameForm :: Html
            -> MForm Handler (FormResult LabyrinthParams, Widget)
newGameForm = renderDivs $ LabyrinthParams
    <$> (LabyrinthFeatures <$> areq checkBoxField (named "pits") (Just True)
                           <*> areq checkBoxField (named "rivers") (Just True))
    <*> areq intField (named "width") Nothing
    <*> areq intField (named "height") Nothing
    <*> areq intField (named "players") Nothing

postNewGameR :: Handler Value
postNewGameR = postForm newGameForm $ \params -> do
    lab <- createLabyrinth params
    gameId <- newId
    res <- update GameList $ AddGame gameId lab
    returnCORSJson (if res then "ok" else "bad game" :: String)

getGameR :: GameId -> Handler Value
getGameR gameId = immediateResponse (GameLog gameId)

data PlayerMove = PlayerMove { pmplayer :: PlayerId
                             , pmmove   :: T.Text
                             }

makeMoveForm :: Html
             -> MForm Handler (FormResult PlayerMove, Widget)
makeMoveForm = renderDivs $ PlayerMove
    <$> areq intField (named "player") Nothing
    <*> areq textField (named "move") Nothing

parseMove' :: T.Text -> Either String Move
parseMove' str = case eitherDecode (BS.fromStrict $ E.encodeUtf8 str) of
    Left err -> parseMove (T.unpack str)
    Right m -> Right m

postMakeMoveR :: GameId -> Handler Value
postMakeMoveR gameId = postForm makeMoveForm $ \playerMove -> do
    let PlayerMove playerId moveStr = playerMove
    case parseMove' moveStr of
        Left err   -> returnCORSJson $ object ["error" .= err]
        Right move -> do
            res <- update (GameLog gameId) $ PerformMove gameId playerId move
            returnCORSJson res

deleteDeleteGameR :: GameId -> Handler Value
deleteDeleteGameR gameId = do
    update GameList $ RemoveGame gameId
    returnCORSJson ("ok" :: String)

getExampleMovesR :: Handler Value
getExampleMovesR = returnCORSJson $ Sensitive True exampleMoves
