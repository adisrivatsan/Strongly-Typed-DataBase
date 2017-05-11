{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as DB

--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

app2 :: WS.ClientApp ()
app2 conn = do
    putStrLn "Connected!"
    -- Fork a thread that writes WS data to stdout
    --WS.sendTextData conn (T.pack "{}")
    _ <- setPath2 conn

    msg <- WS.receiveData conn
    liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    --d <- setPath conn

    WS.sendClose conn ("Bye!" :: Text)


testapp2 :: IO ()
testapp2 = do
  withSocketsDo $ WS.runClient "localhost" 8080 "/" app2
--------------------------------------------------------------------------------
main :: IO ()
main = do
    withSocketsDo $ WS.runClient "localhost" 8080 "/" app

convertValueTextToPath :: Value -> Text -> Text
convertValueTextToPath val path = T.pack("{\"path\": " ++ show (path) ++ "," ++ "\"value\":"
  ++ (show (encode val)) ++ "}")

processPaths ::  Value -> Text -> WS.Connection -> IO ()
processPaths vals path conn = unless (T.null path) $ WS.sendTextData conn
   (convertValueTextToPath vals path)

setPath = processPaths (Data.Aeson.String (T.pack "yes")) (T.pack "d")
setPath2 = processPaths (Data.Aeson.Array (V.singleton(  Data.Aeson.String (T.pack "yo")) ) ) (T.pack "a")
-- need to test this for arrays.
-- make one end point with three arrays
sampleArray :: Value
sampleArray = Data.Aeson.Array (V.singleton(Data.Aeson.String (T.pack "yo"))):
