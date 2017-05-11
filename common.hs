-- Daniel Hanover, Aditya Vatsan, Final Project
module Common (retrieveValueFromPath, validateInsert, Path, Validatable, validate, vempty,put,get) where

import Data.Aeson
import Data.HashMap.Lazy as Hash
import Data.Text

--Public

type Path                    = [Text]

class (FromJSON a, ToJSON a) => Validatable a where
    validate         :: a -> Bool
    vempty           :: a

retrieveValueFromPath :: (Validatable a) => a -> Path -> Maybe Value
retrieveValueFromPath tree path = get path (toJSON tree)

validateInsert :: (Validatable a) => Value -> Path -> a -> (a, Bool)
validateInsert val path tree =
    case fromJSON (put path val (toJSON tree)) of
        Success obj   ->
          if validate obj then (obj, True) else (tree, False)
        Error _       -> (tree, False)

--Private Helpers

emptyObject :: Value
emptyObject = Object Hash.empty

insertHelper :: (Validatable a) => a -> Path -> Value -> Value
insertHelper tree path val = put path val (toJSON tree)

insertValue :: Text -> Value -> Value -> Value
insertValue x obj (Object oldState) = Object (Hash.insert x obj oldState)
insertValue x obj _                 = Object (Hash.insert x obj Hash.empty) --Overwrite previous data with object

-- | Stores data at a path
put :: Path -> Value -> Value -> Value
put [x]    obj oldState              = insertValue x obj oldState
put (x:xs) obj val@(Object oldState) =
    case Hash.lookup x oldState of
      Nothing         -> insertValue x (put xs obj emptyObject) val
      Just subObject  -> insertValue x (put xs obj subObject) val
put (x:xs) obj oldState              = insertValue x (put xs obj emptyObject) oldState
put []     obj oldState              = obj --Overwrite root

-- | Retrieves the data stored at a path
get :: Path -> Value -> Maybe Value
get (x:xs) (Object o) = do
  v <- Hash.lookup x o
  get xs v
get [] v              = Just v
get _ _               = Nothing
