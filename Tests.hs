-- Daniel Hanover, Aditya Vatsan, Final Project
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Tests where

import Test.HUnit hiding (Path)
import Data.Aeson
import Data.Text hiding (foldr)
import Data.HashMap.Lazy as Hash hiding (foldr)
import qualified Data.Vector as V
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.Scientific
import Control.Monad
import Data.List

import Common
import Plugins.RecursiveValidator
import Plugins.FamilyTree
import Main

quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

main :: IO ()
main = do
  recValidatorTest
  famTreeTest
  return ()

genString ::  Gen String
genString = frequency [ (1, return [])
                    , (5, (liftM2 (:)) (genChar) genString) ]

genNonEmptyString ::  Gen String
genNonEmptyString = do
  s <- genString
  if Prelude.length s > 0 then
    return s
  else
    genNonEmptyString

newtype ASCIIChar = ASCII Char

instance Arbitrary ASCIIChar where
  arbitrary = fmap ASCII (elements ['a'..'z'])

genChar :: Gen Char
genChar = elements ['a'..'z']

maxChildren :: Int
maxChildren = 5

genStringTuple :: Gen (Text,Value)
genStringTuple = do
  key <- genNonEmptyString
  val <- genValue
  return (pack key, val)

instance Arbitrary Scientific where
  arbitrary = liftM2 scientific (elements [1..1000]) (elements [0]) --Generate only integers

genNumberValue :: Gen Value
genNumberValue = fmap Number arbitrary

genStringValue :: Gen Value
genStringValue = fmap String arbitrary

genNullValue :: Gen Value
genNullValue = pure Null

genBoolValue :: Gen Value
genBoolValue = fmap Bool arbitrary

genArbitraryKVPair :: Gen [(Text, Value)]
genArbitraryKVPair = frequency [(1, fmap (Prelude.take maxChildren) (listOf genStringTuple)),
                                (maxChildren*2, return [])
                                ]

genObjectValue :: Gen Value
genObjectValue = fmap Object (fmap Hash.fromList genArbitraryKVPair)

genValue :: Gen Value
genValue = oneof [genNumberValue, genStringValue, genBoolValue, genNullValue, genObjectValue]

instance Arbitrary Text where
  arbitrary = fmap pack genNonEmptyString

genPath ::  Gen Path
genPath = liftM2 (:) arbitrary (listOf arbitrary)

instance Arbitrary Value where
    arbitrary = genValue
    shrink (Object o) = case (Hash.toList o) of
      (x:xs) -> [Object (Hash.fromList xs)]
      [] -> []
    shrink a = [id (a)]

genPerson :: Gen Person
genPerson = fmap Person arbitrary

genEmployer :: Gen Employer
genEmployer = fmap Employer arbitrary

instance Arbitrary Employer where
  arbitrary = genEmployer

instance Arbitrary Person where
  arbitrary = genPerson
  shrink = undefined

seedData :: Value
seedData = Object $ Hash.insert ("likesPizza") (Bool True) Hash.empty

sampleDict:: HashMap Text Value
sampleDict = Hash.insert "a" (String "yes") Hash.empty

sampleRoute :: [Text]
sampleRoute = ["a", "a", "b"]

-- | Properties for get and put

prop_getputEmptyDict :: [Text] -> Bool
prop_getputEmptyDict text = let newDict = (Common.put text seedData (Object Hash.empty)) in
  case Common.get text newDict of
    Nothing -> True
    Just a -> a == seedData

prop_getputNonEmptyDict :: [Text] -> Bool
prop_getputNonEmptyDict text  = let newDict = (Common.put text seedData (Object sampleDict)) in
  case Common.get text newDict of
    Nothing -> True
    Just a -> a == seedData

prop_SameRouteDiffValuesEmpty :: Value -> Bool
prop_SameRouteDiffValuesEmpty val = let newDict = (Common.put sampleRoute val (Object sampleDict)) in
  case Common.get sampleRoute newDict of
    Nothing -> True
    Just a -> a == val

-- | TESTS RecursiveValidator

emptyValueObject :: Value
emptyValueObject = Object Hash.empty

genericPath :: Path
genericPath = ["z","x","y"]

personPath :: Path
personPath = ["a"]

employerPath :: Path
employerPath = ["d"]

subrecordPath :: Path
subrecordPath = ["a","c"]

prop_getputShouldEqual :: [Text] -> Value -> Value -> Bool
prop_getputShouldEqual path seed inserted = 
    case Common.get path (Common.put path inserted seed) of
      Nothing -> seed == Null
      Just a  -> a == inserted

-- values that have no specification should be validated correctly
prop_GenericValuesValidated :: Value -> Bool
prop_GenericValuesValidated val = snd (validateInsert val genericPath emptyValueObject)

-- validate person values in general Path
prop_PersonInGeneral :: Person -> Bool
prop_PersonInGeneral person = snd (validateInsert (toJSON person) genericPath emptyValueObject)

-- validate values that have a specification for person
prop_ValidatePerson :: Person -> Bool
prop_ValidatePerson person = snd (validateInsert (toJSON person) personPath emptyValueObject)

-- Objects that are not a person should be rejected
prop_RejectObjNotEmployer :: Value  -> Bool
prop_RejectObjNotEmployer val = not $ snd (validateInsert val employerPath emptyValueObject)

-- Object should still be validated at the sub level as well.
prop_MixedPath :: Person -> Bool
prop_MixedPath person = snd (validateInsert (toJSON person) subrecordPath emptyValueObject)

-- validate values that have a specification for employer
prop_ValidateEmployer :: Employer -> Bool
prop_ValidateEmployer employer = snd (validateInsert (toJSON employer) employerPath emptyValueObject)

recValidatorTest:: IO ()
recValidatorTest = do
  quickCheckN 500 prop_GenericValuesValidated
  quickCheckN 500 prop_PersonInGeneral
  quickCheckN 500 prop_ValidatePerson
  quickCheckN 500 prop_RejectObjNotEmployer
  quickCheckN 500 prop_MixedPath
  quickCheckN 500 prop_ValidateEmployer
  quickCheckN 5000 prop_getputShouldEqual
  return ()


-- | TEST FamilyTree

sampleTreePath:: Path
sampleTreePath = []

sampleTreeValue :: Value
sampleTreeValue = Object $ Hash.fromList [
    ("age", Number 50),
    ("son", Object $ Hash.fromList [
      ("age", Number 26)
    ])
  ]

emptyTree :: FamilyTree Int
emptyTree = vempty

genFamilyTree :: [Int] -> Gen (Maybe (FamilyTree Int))
genFamilyTree (age:ages) = do
  frequency [(1, fmap Just (liftM3 Branch (genFamilyTree ages) (genFamilyTree ages) (pure age))),
                           (5, return Nothing)]
genFamilyTree _ = return Nothing

-- Wrapper to avoid arbitrary typeclass conflict with Maybe
data MaybeFamilyTree a = J (Maybe (FamilyTree Int)) deriving (Show)

instance Arbitrary (MaybeFamilyTree Int) where
  arbitrary = do
    ages <- fmap (Prelude.reverse . sort . nub) $ listOf $ arbitrary
    fmap J (genFamilyTree ages)
  shrink = undefined

prop_testValidTree :: (MaybeFamilyTree Int) -> Bool
prop_testValidTree (J (Just tree)) = snd (validateInsert (toJSON sampleTreeValue) sampleTreePath tree)
prop_testValidTree (J Nothing) = True

prop_testInvalidTree :: Value -> Bool
prop_testInvalidTree val = not $ snd (validateInsert val employerPath emptyTree)

prop_testInsertInRandomTree :: (MaybeFamilyTree Int) -> Bool
prop_testInsertInRandomTree (J (Just tree)) = snd (validateInsert (toJSON tree) sampleTreePath emptyTree)
prop_testInsertInRandomTree (J Nothing) = True

famTreeTest :: IO ()
famTreeTest = do
  quickCheckN 500 prop_testValidTree
  quickCheckN 500 prop_testInvalidTree
  quickCheckN 500 prop_testInsertInRandomTree
  return ()
