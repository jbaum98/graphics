module Pair.Vector (
  dot,
  cross,
  norm
  ) where

import Pair.Triple

infixl 5 `dot`
dot :: (Num a, Show a) => Triple a -> Triple a -> a
dot (Triple a1 a2 a3) (Triple b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3

infixl 5 `cross`
cross :: Num a => Triple a -> Triple a -> Triple a
cross (Triple a1 a2 a3) (Triple b1 b2 b3) = Triple (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)

norm :: Floating a => Triple a -> a
norm (Triple x y z) = sqrt $ x*x + y*y + z*z