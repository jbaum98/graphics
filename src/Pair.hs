{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Pair
Description : Types to represent a fixed number objects of the same type

Includes the 'Pair' and 'Triple'. While lists allow you to map a
function over all of its elements, they can be of any length.
Similarly, tuples are of fixed length and can be uncurried but make no guarantees
about types, so mapping over all elements is impossible.
These types provide fixed-length homogenous containers.
-}
module Pair (Pair(..), Triple(..), uncurryPair, uncurryTriple) where

import Control.DeepSeq
import GHC.Generics

-- |Represents two objects of the same type
data Pair a = Pair a a
            deriving (Show, Eq, Generic)

instance NFData a => NFData (Pair a)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure f = Pair f f
  Pair f1 f2 <*> Pair a b = Pair (f1 a) (f2 b)

instance Foldable Pair where
  foldMap m (Pair a1 a2) = m a1 `mappend` m a2

-- |Represents three objects of the same type
data Triple a = Triple a a a
              deriving (Show, Eq, Generic)

instance NFData a => NFData (Triple a)

instance Functor Triple where
  fmap f (Triple a b c) = Triple (f a) (f b) (f c)

instance Applicative Triple where
  pure f = Triple f f f
  Triple f1 f2 f3 <*> Triple a b c = Triple (f1 a) (f2 b) (f3 c)

instance Foldable Triple where
  foldMap m (Triple a1 a2 a3) = m a1 `mappend` m a2 `mappend` m a3

-- |Converts a function taking two arguments of the same type
-- to a function taking single 'Pair'
uncurryPair :: (a -> a -> b) -> Pair a -> b
uncurryPair f (Pair a1 a2) = f a1 a2

-- |Converts a function taking three arguments of the same type
-- to a function taking single 'Triple'
uncurryTriple :: (a -> a -> a -> b) -> Triple a -> b
uncurryTriple f (Triple a1 a2 a3) = f a1 a2 a3
