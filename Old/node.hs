{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Node where

import Data.Aeson (Value)

class Show b => Node a b where
  keys             :: a -> [b]
  --childForKey      :: a -> b -> Maybe c
  --valueForKey      :: a -> b -> Maybe Value
class Validatable a where
	validator      :: a -> Bool
