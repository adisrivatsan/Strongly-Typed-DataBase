{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module DeveloperCode (TypeStructureValue (TS),Person (Person), testTemplate,serverType) where
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



data TypeStructureValue = TS (HashMap Text TypeStructureValue) (Value -> Bool)

data Person =
  Person { likesPizza :: Bool
           } deriving (Show,Generic)

instance FromJSON Person
instance ToJSON Person

-- polymorphism
testTemplate :: (FromJSON a) => Value -> Maybe a
testTemplate val = (decode (encode val))

isPerson :: Value -> Maybe Person
isPerson = testTemplate


genTypeStructureValue :: (Value -> Maybe a) -> TypeStructureValue
genTypeStructureValue fun = TS Hash.empty (isJust.fun)

serverType :: TypeStructureValue
serverType = TS (Hash.fromList
  [
  (Data.Text.pack("a"), genTypeStructureValue isPerson),
  (Data.Text.pack("b"), genTypeStructureValue isPerson),
  (Data.Text.pack("c"), genTypeStructureValue isPerson)
  ]) (isJust.isPerson)
