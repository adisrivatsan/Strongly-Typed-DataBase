-- NOT USING FILE 

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
import GHC.Generics
import Data.Typeable
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List as List
import Data.Char as Char
import Test.HUnit
import Data.Aeson
import Data.Text
import Data.Text.Lazy.Encoding
import Data.HashMap.Lazy as Hash
import Data.Vector
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import Test.QuickCheck
import Text.Read as Read2
--import Control.Lens
--------------------------
--import Server as Server

--data Chat = Chat { message :: String, senderID :: String, recipientID :: String } deriving (Show,Generic)

data Subscription a = Subscription [a]  -- The monadic type

instance Monad Subscription where
  return x = Subscription [x]
  (Subscription xs) >>= f = Subscription (List.foldl (\x acc (Subscription x) -> x : acc) [] (List.map f xs))


-- listen "path"   . filter (>8) .map (+4).bind (\x -> )
-- listen "path/a" . filter (<6) .map (+14).bind (\y -> )

--instance FromJSON Chat
--instance ToJSON Chat

--main :: IO ()
--main = do
    --server  <- Server.init
    --((server.subscribe "/groups/GHC-Forum") (server.subscribe "/groups/GHC-Forum2"))
    --x <- (server.subscribe "/groups/GHC-Forum") . filter (>8) . map (+1)
    --do something with x

-- listen :: String -> (Text -> IO ()) -> IO ()
