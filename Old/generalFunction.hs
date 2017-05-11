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
import qualified Data.ByteString.Lazy as B
import Test.QuickCheck
import Text.Read as Read2


type Fresh = HashMap Text Value

textArray :: [Text]
textArray = [Data.Text.pack "a", Data.Text.pack "b"]


seedData :: Value
seedData = Object $ Hash.insert (Data.Text.pack "likesPizza") (Data.Aeson.Bool True) Hash.empty

samplePerson :: Person
samplePerson = Person True

put :: [Text] -> Fresh -> Fresh -> Fresh
put [x] obj oldState = Hash.insert x (Object(obj)) oldState
put (x:xs) obj oldState = case (Hash.lookup x oldState) of
  Nothing  -> Hash.insert x (Object (put xs obj Hash.empty)) oldState
  Just y ->  case y of
    Object a -> Hash.insert x (Object (put xs obj a)) oldState
    _ -> Hash.insert x (Object (put xs obj Hash.empty)) oldState
put [] _ oldState = oldState

get3 :: [Text] -> HashMap Text Value -> Maybe Value
get3 [x] oldState = Hash.lookup x oldState
get3 (x:xs) oldState = case (Hash.lookup x oldState) of
  Nothing -> Nothing
  Just y -> case y of
    Object a -> get3 xs a
    _ -> Nothing
get3 [] _ = Nothing

--sampleObj :: HashMap Text Value
--sampleObj = Hash.insert (pack "key") (String (pack "value")) Hash.empty

tListString :: [String] -> [Text]
tListString = List.map Data.Text.pack

--clientStore

putTest :: Test
putTest = undefined
-- properties in overriding every data type other than object.

testInsert :: String -> Int -> HashMap String Int -> HashMap String Int
testInsert s i old = Hash.insert s i old
-- array of strings -> (Json Object) by Aeson ->
-- findNestedKey :: Text -> HashMap Text Value -> Maybe Value
-- findNestedKey key obj = case obj.keys of
--   (x:xs) -> if key == x then Hash.lookup x obj else do
--     val <- Hash.lookup x obj
--     case val of
--       Object a -> case findNestedKey key a of
--         Just a -> return a
--         Nothing ->

data Color = Red | Green | Blue
  deriving (Show)

handleRead :: String -> Maybe Color
handleRead st = undefined--(read st) :: Color
instance Read Color where
    -- readsPrec is the main function for parsing input
    readsPrec _ value =
        -- We pass tryParse a list of pairs.  Each pair has a string
        -- and the desired return value.  tryParse will try to match
        -- the input to one of these strings.
        tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
        where tryParse [] = []    -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                      -- Compare the start of the string to be parsed to the
                      -- text we are looking for.
                      if (Prelude.take (Prelude.length attempt) value) == attempt
                         -- If we have a match, return the result and the
                         -- remaining input
                         then [(result, Prelude.drop (Prelude.length attempt) value)]
                         -- If we don't have a match, try the next pair
                         -- in the list of attempts.
                         else tryParse xs



readInput :: String -> Maybe Color
readInput st = case (read st) :: Color of
  Red -> Just Red
  Blue -> Just Blue
  Green -> Just Green


maybeRead :: Read Color => String -> Maybe Color
maybeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing
--we are keeping space

data Person2 = Person2 {
     age  :: Int
    } deriving (Show,Read)



maybeReadGeneral :: Read a => String -> Maybe a
maybeReadGeneral s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

-- {
--    "a": Person,
--    "b": TrashCan,
--    "c": {
--       "d": Person
--     }
-- }

--checkString :: (FromJSON t) => B.ByteString -> Maybe t
--checkString st = (Data.Aeson.decode st) :: Maybe t

data Person =
  Person { likesPizza :: Bool
           } deriving (Show,Generic)

instance FromJSON Person
instance ToJSON Person


testInter :: (FromJSON a) => Value -> Maybe a
testInter val= (decode (encode val))

isPerson :: Value -> Maybe Person
isPerson = testInter




class ReadCheck a where
  checktype :: String -> Maybe a
instance ReadCheck Person2 where
  checktype st = (readMaybe st) :: Maybe Person2

--polymorphism!!!
readAny :: (Read a) => String -> Maybe a
readAny st = (readMaybe st)
readPerson :: String -> Maybe Person2
readPerson = readAny

decodeAny :: Value -> Maybe a
decodeAny val = undefined

data Expr = Var String
           | Neg Expr
           | Add Expr Expr
           deriving (Show,Data,Typeable)

data ValWrapper = V Value deriving(Show,Data,Typeable)

valStrings :: ValWrapper -> [Text]
valStrings (V x) = [y | Data.Aeson.String y <- universe x]

valString2 :: Value -> [Text]
valString2 x = [y | Data.Aeson.String y <- universe x]


-- using uniplate haskell

--polymorphic types
allVars :: Expr -> [a]
allVars = undefined

variables :: Expr -> [String]
variables x = [y | Var y <- universe x]

convertExpr :: Expr -> Expr
convertExpr (Var a) = Var "hello"
convertExpr (Neg b) = convertExpr b
convertExpr (Add a b) = convertExpr b




--test
