{-# LANGUAGE BangPatterns #-}

module Data.Pair.Vector (
  dot,
  cross,
  norm,
  normalize
  ) where

import Data.Pair.Triple

infixl 5 `dot`
dot :: (Num a, Show a) => Triple a -> Triple a -> a
dot (Triple !a1 !a2 !a3) (Triple !b1 !b2 !b3) = a1 * b1 + a2 * b2 + a3 * b3
{-# INLINE dot #-}

infixl 5 `cross`
cross :: Num a => Triple a -> Triple a -> Triple a
cross (Triple !a1 !a2 !a3) (Triple !b1 !b2 !b3) = Triple (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)
{-# INLINE cross #-}

norm :: Floating a => Triple a -> a
norm (Triple !x !y !z) = sqrt $ x*x + y*y + z*z
{-# INLINE norm #-}

normalize :: Floating a => Triple a -> Triple a
normalize v = v / (pure $ norm v)
