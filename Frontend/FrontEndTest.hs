-- Daniel Hanover, Aditya Vatsan, Final Project
{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Aeson
import Data.Text as T
import Data.HashMap.Lazy as Hash
import qualified Data.Vector as V
import Test.QuickCheck

import Frontend.ClientInterface

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

payload:: Value -> Maybe Text
payload val = Just $ T.pack "works"

testValPath:: Maybe Value
testValPath = Just $ Object $ Hash.fromList [
  ("x/y", String "hello") ]

testValSingPath:: Maybe Value
testValSingPath = Just $ Object $ Hash.fromList [
  ("z", String "no path") ]

tCheckPathMaybe :: Test
tCheckPathMaybe = "checkPathMaybe" ~: TestList [
  checkPathMaybe ["x","y"] payload testValPath ~?= Just (T.pack "works"),
  checkPathMaybe ["x","b"] payload testValPath ~?= Nothing,
  checkPathMaybe ["z"] payload testValSingPath ~?= Just (T.pack "works"),
  checkPathMaybe ["e"] payload testValSingPath ~?= Nothing ]
