{-|
Module      : Pair
Description : Types to represent a fixed number objects of the same type

Includes the 'Pair' and 'Triple'. While lists allow you to map a function over
all of its elements, they can be of any length. Similarly, tuples are of fixed
length and can be uncurried but make no guarantees about types, so mapping over
all elements is impossible. These types provide fixed-length homogenous
containers, and therefore have can have 'Functor' and other instances that
cannot be written for tuples.
-}
module Data.Pair (
  Pair(..),
  Triple(..),
  -- * Uncurrying
  uncurryPair,
  uncurryTriple,
  -- * Vector Operations
  dot,
  cross,
  norm,
  normalize
) where

import Data.Pair.Pair
import Data.Pair.Triple
import Data.Pair.Vector
