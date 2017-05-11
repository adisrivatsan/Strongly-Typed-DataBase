-- Daniel Hanover, Aditya Vatsan, Final Project
{-# LANGUAGE OverloadedStrings #-}

module FrontendMain where

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Data.List           as LS
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as Hash

import Frontend.ClientInterface
import Frontend.Helpers
import Plugins.RecursiveValidator

data IOResult = T (IO (Either String Text))

app :: WS.ClientApp ()
app conn = do
    Prelude.putStrLn "Connected!"

    -- Listen on interesting server locations
    installListeners conn $ map fst clientActions

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
      text <- WS.receiveData conn :: IO Text
      let proc = processText text
      let msg = decode proc :: Maybe Value
      t <- readInIO $ exPathTotal (clientActions) msg
      liftIO $ T.putStrLn t

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)
--------------------------------------------------------------------------------
main :: IO ()
main =
    withSocketsDo $ WS.runClient "localhost" 8080 "/" app

installListeners :: WS.Connection -> [[Text]] -> IO ()
installListeners conn (path:paths) = do
  WS.sendTextData conn $ addBrackets ((addQuotes "path") +|+ (T.pack ": ") +|+ addQuotes (T.intercalate "/" path))
  installListeners conn paths
installListeners _ _ = return ()


checkPathMaybe :: [Text] -> (Value -> Maybe Text) -> Maybe Value -> Maybe Text
checkPathMaybe userPath func msg = do
  val <- msg
  case val of
    (Object o) -> case Hash.keys o of
      [x] -> if userPath == T.splitOn (T.pack "/") x then do
        el <- Hash.lookup x o
        func el else Nothing
      _ -> Nothing
    _   -> Nothing

exPathTotal :: [([Text], Value -> Maybe Text)] -> Maybe Value -> IOResult
exPathTotal ((x,y):xs) val = case checkPathMaybe x y val of
    Nothing -> exPathTotal xs val
    Just a -> T $ return $ Right a
exPathTotal [] _ = T $ return $ Left "type mismatch"

readInIO :: IOResult -> IO Text
readInIO (T x) = do
  a <- x
  case a of
    Left e -> return $ T.pack e
    Right s -> return s

addBrackets :: Text -> Text
addBrackets t = T.snoc  (T.cons '{' t) '}'

addQuotes:: Text -> Text
addQuotes t = T.snoc (T.cons '\"' t) '\"'

processText :: Text -> B.ByteString
processText t = case T.breakOn (T.pack ":") t of
  (x,y) -> WS.toLazyByteString $  addBrackets $ T.append (addQuotes x) y
