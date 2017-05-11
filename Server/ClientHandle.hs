-- Daniel Hanover, Aditya Vatsan, Final Project
module Server.ClientHandle (ClientHandle(ClientHandle), Listeners) where

import Data.Hashable (Hashable, hash, hashWithSalt)
import Network.WebSockets
import Data.HashMap.Lazy as Hash

import Common


newtype ClientHandle          = ClientHandle { client :: (Int, Connection) }
type Listeners                = HashMap ClientHandle [Path]

instance Eq ClientHandle where
  c1 == c2 = fst (client c1) == fst (client c2)
instance Ord ClientHandle where
  c1 <= c2 = fst (client c1) <= fst (client c2)
instance Hashable ClientHandle where
  hash c = fst (client c)
  hashWithSalt i c = i*100 + fst (client c)
