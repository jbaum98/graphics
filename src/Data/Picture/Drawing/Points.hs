{-# LANGUAGE FlexibleContexts, BangPatterns, MagicHash, UnboxedTuples #-}

module Data.Picture.Drawing.Points (
  unsafeRawWritePoint,
  rawWritePoint,
  writePoint,
  setPointColor,
  setColor,
  reflect
  ) where

import Control.Monad

import Data.Primitive.ByteArray
import Control.Monad.Primitive

import Data.Color
import Data.Pair
import Data.Picture.Picture

writePoint, rawWritePoint, unsafeRawWritePoint :: PrimMonad m => Color -> Pair Int -> MPicture (PrimState m) -> m ()
unsafeRawWritePoint (Triple !r !g !b) !point (MPicture !rs !gs !bs !s) = do
  let !i = encode s point
  writeByteArray rs i r
  writeByteArray gs i g
  writeByteArray bs i b
{-# INLINE unsafeRawWritePoint #-}

rawWritePoint !color !point !pic = do
  s <- getSizeM pic
  when (point `inRange` s) $
    unsafeRawWritePoint color point pic
{-# INLINE rawWritePoint #-}

writePoint !color !point !pic = do
  Pair _ yMax <- getSizeM pic
  let point' = reflect yMax point
  rawWritePoint color point' pic
{-# INLINE writePoint #-}

setPointColor :: Color -> Pair Int -> Picture -> Picture
setPointColor _ point pic | point `inRange` getSize pic = pic
setPointColor !color !point !pic = mutPic pic $ \p -> do
  unsafeRawWritePoint color point' p
  return p
  where
    point' = reflect yMax point
    Pair _ yMax = getSize pic
{-# INLINE setPointColor #-}

reflect :: Num a => Int -> Pair a -> Pair a
reflect !yMax (Pair !x !y) = Pair x (fromIntegral yMax - y - 1)
{-# SPECIALIZE reflect :: Int -> Pair Int -> Pair Int #-}
{-# SPECIALIZE reflect :: Int -> Pair Double -> Pair Double #-}
{-# INLINABLE reflect #-}

-- |Set every 'Point' in a list to a single 'Color'
setColor :: Color -> [Pair Int] -> Picture -> Picture
setColor !color !points !pic = mutPic pic $ \mp -> do
  forM_ points $ \point -> rawWritePoint color (reflect yMax point) mp
  return mp
  where
    Pair _ yMax = getSize pic
