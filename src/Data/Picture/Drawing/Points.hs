{-# LANGUAGE FlexibleContexts, BangPatterns, MagicHash, UnboxedTuples #-}

module Data.Picture.Drawing.Points (
  writePoint,
  rawWritePoint,
  unsafeRawWritePoint,
  writePoints,
  reflect
  ) where

import Control.Monad

import Data.Primitive.ByteArray
import Control.Monad.Primitive

import Data.Color
import Data.Pair
import Data.Picture.Picture

writePoint, rawWritePoint, unsafeRawWritePoint :: PrimMonad m => Color -> Int -> Int -> Double -> Picture (PrimState m) -> m ()

-- | Write a point to a 'Picture' without reflecting or bounds checking.
unsafeRawWritePoint (Triple !r !g !b) !x !y !z (Picture !rs !gs !bs !zs !s) = do
  let !i = encode s $ Pair x y
  oldZ <- readByteArray zs i
  when (z >= oldZ) $ do
    writeByteArray rs i r
    writeByteArray gs i g
    writeByteArray bs i b
    writeByteArray zs i z
{-# INLINE unsafeRawWritePoint #-}

-- | Write a point to a 'Picture' without reflecting.
rawWritePoint color !x !y !z !pic =
  when (Pair x y `inRange` s) $
    unsafeRawWritePoint color x y z pic
  where s = getSize pic
{-# INLINE rawWritePoint #-}

-- | Write a point to a 'Picture'.
writePoint !color !x !y !z !pic = do
  let Pair x' y' = reflect yMax $ Pair x y
  rawWritePoint color x' y' z pic
  where Pair _ yMax = getSize pic
{-# INLINE writePoint #-}

-- | Reflect the point so the origin is the bottom left
reflect :: Num a => Int -> Pair a -> Pair a
reflect !yMax (Pair !x !y) = Pair x (fromIntegral yMax - y - 1)
{-# SPECIALIZE reflect :: Int -> Pair Int -> Pair Int #-}
{-# SPECIALIZE reflect :: Int -> Pair Double -> Pair Double #-}
{-# INLINABLE reflect #-}

-- | Set every 'Point' in a list to a single 'Color'
writePoints :: PrimMonad m => Color -> [(Pair Int, Double)] -> Picture (PrimState m) -> m ()
writePoints !color !points !mp =
  forM_ points $ \(point,z) -> uncurryPair (rawWritePoint color) (reflect yMax point) z mp
  where
    Pair _ yMax = getSize mp
