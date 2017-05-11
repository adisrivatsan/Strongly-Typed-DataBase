-- Daniel Hanover, Aditya Vatsan, Final Project
{-# LANGUAGE OverloadedStrings #-}

module Server.Core (initializeServer) where

import Prelude
import qualified Data.List as List
import Data.HashMap.Lazy as Hash
import Data.Aeson
import Data.Text as Text
import qualified Data.ByteString.Lazy as B
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.Text.Encoding
import Network.WebSockets
import Control.Monad
import Configurable.ServerStoreFormat

import Server.ClientHandle
import Common
import Plugins.RecursiveValidator
import Plugins.FamilyTree

--State

data ServerStore = S { store :: ServerStoreFormat, clients :: [ClientHandle], listeners :: Listeners }

newState :: ServerStore
newState = S vempty [] Hash.empty

-- State Actions

addClient :: ClientHandle -> ServerStore -> ServerStore
addClient client (S store clients l) = S store (client:clients) l

addListener :: (ClientHandle, Path) -> ServerStore -> ServerStore
addListener (client, path) (S store clients l) = S store clients $ insertWith (++) client [path] l

notifyListeners :: Path -> Value -> Listeners -> IO [()]
notifyListeners location value listeners = mapM
  (\(ClientHandle (_, connection), v) ->
    Control.Monad.when (location `elem` v) $
      let pathStr = append (intercalate "/" location) ": " in
      sendTextData connection (append pathStr $ decodeValue $ Just value)
  )
  (toList listeners)

--Command Dictionary

data Command = Command { path :: Text, value :: Maybe Value }

instance FromJSON Command where
  parseJSON = withObject "Command" $ \o ->
    Command <$> o .: "path" <*> o .:? "value"

-- Helpers

decodeValue :: Maybe Value -> Text
decodeValue v = decodeUtf8 $ B.toStrict $ encode v

locationFromPath :: Text -> Path
locationFromPath "/" = []
locationFromPath "" = []
locationFromPath p = List.filter (not . Text.null) $ splitOn "/" p

processSubscribeCommand :: Text -> MVar ServerStore -> Connection -> Int -> IO ()
processSubscribeCommand pth state connection counter =
  let location = locationFromPath pth in
  let pathStr = append (intercalate "/" location) ": " in
  do
    (S fresh _ _) <- readMVar state
    sendTextData connection $ append pathStr $ decodeValue (retrieveValueFromPath fresh location)
    modifyMVar_ state $ \s ->
      return (addListener (ClientHandle (counter, connection), location) s)

processPutCommand :: Text -> Value -> MVar ServerStore -> Connection -> Int -> IO ()
processPutCommand pth val state connection counter =
  let location = locationFromPath pth in
  do
    (S fresh _ _) <- readMVar state
    case validateInsert val location fresh of
      (a, True)  ->
                    modifyMVar_ state $ \(S fresh clients listeners) -> do
                      notifyListeners location val listeners
                      sendTextData connection ("Great Success!" :: Text)
                      return (S a clients listeners)
      (a, False) ->
                    sendTextData connection ("Invalid Type" :: Text)

-- Core Socket Logic

initializeServer :: IO ()
initializeServer = do
  state   <- newMVar newState
  counter <- newMVar 0
  runServer "127.0.0.1" 8080 (handleConnection state counter)

handleConnection :: MVar ServerStore -> MVar Int -> ServerApp
handleConnection state counter pending = do
  connection <- acceptRequest pending
  count      <- readMVar counter
  forkPingThread connection 30
  modifyMVar_ state $ \s -> pure $ addClient (ClientHandle (count, connection)) s
  processCommandLoop (ClientHandle (count, connection)) state

processCommandLoop :: ClientHandle -> MVar ServerStore -> IO b
processCommandLoop (ClientHandle (counter, connection)) state = do
  (Text text) <- receiveDataMessage connection
  let cmd  = decode text :: Maybe Command in
    case cmd of
        Nothing -> sendTextData connection ("Invalid Command" :: Text)
        Just (Command path Nothing)     -> processSubscribeCommand path state connection counter
        Just (Command path (Just val))  -> processPutCommand       path val state connection counter
  processCommandLoop (ClientHandle (counter, connection)) state
