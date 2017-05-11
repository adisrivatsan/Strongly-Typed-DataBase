-- Daniel Hanover, Aditya Vatsan, Final Project
{-# LANGUAGE OverloadedStrings #-}

module Frontend.ClientInterface (clientActions) where

import Data.Aeson
import Data.Text
import Data.HashMap.Lazy as Hash

import Plugins.RecursiveValidator
import Frontend.Helpers

clientActions :: [([Text], (Value -> Maybe Text))]
clientActions = [
  (["ZestosPizza", "manager"], newZestosBoss),
  (["AllegrosPizza", "manager"], newAllegrosBoss),
  (["AllegrosPizza", "employees", "clerk"], newAllegrosClerk)
  ]

newZestosBoss :: Value -> Maybe Text
newZestosBoss json = case (fromJSON json) of
    Success boss -> Just $ ("Watch out for the new Zesto's Don: " +|+ (pack $ name boss))
    Error   err  -> Just $ pack err

newAllegrosBoss :: Value -> Maybe Text
newAllegrosBoss json = case (fromJSON json) of
    Success boss -> Just $ ("Watch out for the new Allegro's Don: " +|+ (pack $ name boss))
    Error   err  -> Just $ pack err

newAllegrosClerk :: Value -> Maybe Text
newAllegrosClerk json = case (fromJSON json) of
    Success clerk ->
       let niceString = if isNice clerk then "nice. " else "not nice. " in
       Just $ ("Allegro's just hired another one. " +|+ (pack $ title clerk) +|+ " is " +|+ niceString)
    Error   err  -> Just $ pack err

--Helper
