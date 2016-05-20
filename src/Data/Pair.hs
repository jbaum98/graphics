{-|
Module      : Pair
Description : Types to represent a fixed number objects of the same type

Includes the 'Pair' and 'Triple'. While lists allow you to map a
function over all of its elements, they can be of any length.
Similarly, tuples are of fixed length and can be uncurried but make no guarantees
about types, so mapping over all elements is impossible.
These types provide fixed-length homogenous containers.
-}
module Data.Pair (
  module X
) where

import Data.Pair.Pair as X
import Data.Pair.Triple as X
import Data.Pair.Vector as X
