{-# LANGUAGE BangPatterns #-}

module Data.Pair.Vector (
  dot,
  cross,
  norm,
  normalize
  ) where

import Data.Pair.Triple

-- |Compute the dot or scalar product of two vectors.
infixl 5 `dot`
dot :: Num a => Triple a -> Triple a -> a
dot (Triple !a1 !a2 !a3) (Triple !b1 !b2 !b3) = a1 * b1 + a2 * b2 + a3 * b3
{-# SPECIALIZE INLINE dot :: Triple Int -> Triple Int -> Int #-}
{-# SPECIALIZE INLINE dot :: Triple Double -> Triple Double -> Double #-}

-- |Compute the cross or vector product of two vectors.
infixl 5 `cross`
cross :: Num a => Triple a -> Triple a -> Triple a
cross (Triple !a1 !a2 !a3) (Triple !b1 !b2 !b3) = Triple (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)
{-# SPECIALIZE INLINE cross :: Triple Int -> Triple Int -> Triple Int #-}
{-# SPECIALIZE INLINE cross :: Triple Double -> Triple Double -> Triple Double #-}

-- |Compute the Euclidean norm of a vector.
norm :: Floating a => Triple a -> a
norm (Triple !x !y !z) = sqrt $ x*x + y*y + z*z
{-# SPECIALIZE INLINE norm :: Triple Double -> Double #-}

-- |Rescale the original vector so it's length is 1 and points in the same direction.
normalize :: Floating a => Triple a -> Triple a
normalize v = v / (pure $ norm v)
{-# SPECIALIZE normalize :: Triple Double -> Triple Double #-}
