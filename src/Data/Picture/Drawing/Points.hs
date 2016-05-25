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

writePoint, rawWritePoint, unsafeRawWritePoint :: PrimMonad m => Color -> Int -> Int -> MPicture (PrimState m) -> m ()
unsafeRawWritePoint (Triple !r !g !b) !x !y (MPicture !rs !gs !bs !s) = do
  let !i = encode s $ Pair x y
  writeByteArray rs i r
  writeByteArray gs i g
  writeByteArray bs i b
{-# INLINE unsafeRawWritePoint #-}

rawWritePoint !color x y !pic = do
  s <- getSizeM pic
  when (Pair x y `inRange` s) $
    unsafeRawWritePoint color x y pic
{-# INLINE rawWritePoint #-}

writePoint !color !x !y !pic = do
  Pair _ yMax <- getSizeM pic
  let Pair x' y' = reflect yMax $ Pair x y
  rawWritePoint color x' y' pic
{-# INLINE writePoint #-}

setPointColor :: Color -> Int -> Int -> Picture -> Picture
setPointColor _ !x !y pic | Pair x y `inRange` getSize pic = pic
setPointColor !color !x !y !pic = mutPic pic $ \p -> do
  unsafeRawWritePoint color x' y' p
  return p
  where
    Pair x' y' = reflect yMax $ Pair x y
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
  forM_ points $ \point -> uncurryPair (rawWritePoint color) (reflect yMax point) mp
  return mp
  where
    Pair _ yMax = getSize pic
