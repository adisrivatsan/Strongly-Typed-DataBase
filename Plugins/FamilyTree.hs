-- Daniel Hanover, Aditya Vatsan, Final Project
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Plugins.FamilyTree (FamilyTree(Branch)) where

import Data.Aeson
import Data.Foldable
import Data.List
import Data.Text hiding (map)

import Utils.AesonAdditions
import Common

data FamilyTree a = Branch { son :: Maybe (FamilyTree a), daughter :: Maybe (FamilyTree a), age :: a }
  deriving (Show)

instance FromJSON (FamilyTree Int) where
    parseJSON = withObject "FamilyTree" $ \o ->
        Branch <$> o .:? "son"
               <*> o .:? "daughter"
               <*> o .:  "age"
               <*  o .:!! ["son", "daughter", "age"]

instance ToJSON (FamilyTree Int) where
    toJSON (Branch son daughter age) = object ["son" .= son, "daughter" .= daughter, "age" .= age]

instance Validatable (FamilyTree Int) where
    vempty                                    = Branch Nothing Nothing 0
    validate   (Branch (Just a) (Just b) c)   = validate a && validate b && age a < c && age b < c
    validate   (Branch (Just a) Nothing c)    = validate a && age a < c
    validate   (Branch Nothing (Just b) c)    = validate b && age b < c
    validate   (Branch Nothing Nothing c)     = True
