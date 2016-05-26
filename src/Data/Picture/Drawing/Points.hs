{-# LANGUAGE FlexibleContexts, BangPatterns, MagicHash, UnboxedTuples #-}

module Data.Picture.Drawing.Points (
  unsafeRawWritePoint,
  rawWritePoint,
  writePoint,
  writePoints,
  reflect
  ) where

import Control.Monad

import Data.Primitive.ByteArray
import Control.Monad.Primitive

import Data.Color
import Data.Pair
import Data.Picture.Picture

writePoint, rawWritePoint, unsafeRawWritePoint :: PrimMonad m => Color -> Int -> Int -> Picture (PrimState m) -> m ()
unsafeRawWritePoint (Triple !r !g !b) !x !y (Picture !rs !gs !bs !s) = do
  let !i = encode s $ Pair x y
  writeByteArray rs i r
  writeByteArray gs i g
  writeByteArray bs i b
{-# INLINE unsafeRawWritePoint #-}

rawWritePoint !color x y !pic = do
  when (Pair x y `inRange` s) $
    unsafeRawWritePoint color x y pic
  where s = getSize pic
{-# INLINE rawWritePoint #-}

writePoint !color !x !y !pic = do
  let Pair x' y' = reflect yMax $ Pair x y
  rawWritePoint color x' y' pic
  where Pair _ yMax = getSize pic
{-# INLINE writePoint #-}

reflect :: Num a => Int -> Pair a -> Pair a
reflect !yMax (Pair !x !y) = Pair x (fromIntegral yMax - y - 1)
{-# SPECIALIZE reflect :: Int -> Pair Int -> Pair Int #-}
{-# SPECIALIZE reflect :: Int -> Pair Double -> Pair Double #-}
{-# INLINABLE reflect #-}

-- |Set every 'Point' in a list to a single 'Color'
writePoints :: PrimMonad m => Color -> [Pair Int] -> Picture (PrimState m) -> m ()
writePoints !color !points !mp =
  forM_ points $ \point -> uncurryPair (rawWritePoint color) (reflect yMax point) mp
  where
    Pair _ yMax = getSize mp
