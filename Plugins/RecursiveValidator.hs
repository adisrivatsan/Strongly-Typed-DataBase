-- Daniel Hanover, Aditya Vatsan, Final Project
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Plugins.RecursiveValidator (ValueStore,Person(isNice,title),Employer(name)) where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.HashMap.Lazy as Hash
import Data.Maybe
import Data.Hashable (Hashable)

import Common

--Must wrap `Value` in a new synonym type so ServerStoreFormat.hs doesn't need to import Data.Aeson
type ValueStore = Value

type Validator = (Value) -> Bool

data RecursiveValidator = V { subvalidators :: HashMap Text RecursiveValidator, vfunction :: Value -> Bool }

--Utility

rvempty :: RecursiveValidator
rvempty = V Hash.empty (const True)

isSuccessful :: Result a -> Bool
isSuccessful (Success _) = True
isSuccessful (Error   _) = False

rvOr :: RecursiveValidator -> RecursiveValidator -> RecursiveValidator
rvOr (V h1 f1) (V h2 f2) = V (intersectionWith rvOr h1 h2) (\v -> f1 v || f2 v)

rvAnd :: RecursiveValidator -> RecursiveValidator -> RecursiveValidator
rvAnd (V h1 f1) (V h2 f2) = V (unionWith rvAnd h1 h2) (\v -> f1 v && f2 v)

--rv_foldl :: (RecursiveValidator -> RecursiveValidator -> RecursiveValidator) -> [RecursiveValidator] -> RecursiveValidator
--rv_foldl f foldable = V () (foldl (\succ vb -> (\v -> f (succ v) (vb v))) True (Prelude.map vfunction foldable))

record :: (Value -> Result a) -> RecursiveValidator
record fun = V Hash.empty (isSuccessful.fun)

rvListOf :: (Value -> Result a) -> RecursiveValidator
rvListOf fun = V Hash.empty (\v ->
  case v of
    (Object obj) -> Prelude.all (isSuccessful.fun) (elems obj)
    _            -> False
  )

unrestricted :: RecursiveValidator
unrestricted = V Hash.empty (const True)

toBottomLevelValidator :: (Value -> Bool) -> RecursiveValidator
toBottomLevelValidator = V Hash.empty

--Developer Record Structures

data Person =
  Person { likesPizza :: Bool,
           isNice     :: Bool,
           title      :: String
           } deriving (Show,Generic)
instance FromJSON Person
instance ToJSON   Person

data Employer =
  Employer {
   name :: String
} deriving (Show,Generic)
instance FromJSON Employer
instance ToJSON   Employer

isPerson :: Value -> Result Person
isPerson = fromJSON

isEmployer :: Value -> Result Employer
isEmployer = fromJSON

isString :: Value -> Bool
isString (String _) = True
isString _          = False


subHelper :: RecursiveValidator -> Value -> Bool
subHelper tmap@(V tmaphash check) val@(Object obj) = check val
  && Prelude.foldl (\succ (key, value) -> succ && subHelper (fromMaybe rvempty (Hash.lookup key tmaphash)) value) True (Hash.toList obj)
subHelper (V tmap check) val = check val

instance Validatable Value where
    vempty    = Null
    validate  = subHelper topLevelValidator

(*|*) :: [(Text, RecursiveValidator)] -> Validator -> RecursiveValidator
(*|*) a b = V (Hash.fromList a) b

(<--) :: Text -> RecursiveValidator -> (Text, RecursiveValidator)
(<--) = (,)

-- | Bind an array of validators to the children of a path
-- | Note that this sets the validator for the current path to `const True`,
-- | so the only validators for this path are the children validators
(<|--) :: Text -> [(Text, RecursiveValidator)] -> (Text, RecursiveValidator)
(<|--) t a = (,) t (a *|* const True)

topLevelValidator :: RecursiveValidator
topLevelValidator =
  [
      "a" <-- (record isPerson `rvOr` rvListOf isPerson),
      "ZestosPizza" <|-- [
        "employees" <-- rvListOf isPerson,
        "manager"  <-- record isEmployer
      ],
      "AllegrosPizza" <|-- [
        "employees" <-- rvListOf isPerson,
        "manager"  <-- record isEmployer
      ],
      "b" <-- unrestricted,
      "c" <-- toBottomLevelValidator isString,
      "d" <-- record isEmployer
  ]
  *|* const True
