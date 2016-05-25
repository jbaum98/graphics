{-# LANGUAGE MagicHash, UnboxedTuples, LambdaCase, RankNTypes, BangPatterns #-}

{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Data.Picture.Picture (
    Picture(..), MPicture(..),
    unsafeFreezePic, unsafeThawPic,
    getSize, getSizeM,
    encode, decode, inRange,
    mutPic,
    indexM,
    (!)
    ) where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Control.Monad.ST

import Data.Color
import Data.Pair

data Picture = Picture {-# UNPACK #-} !ByteArray {-# UNPACK #-} !ByteArray {-# UNPACK #-} !ByteArray !(Pair Int)
data MPicture s = MPicture {-# UNPACK #-} !(MutableByteArray s) {-# UNPACK #-} !(MutableByteArray s) {-# UNPACK #-} !(MutableByteArray s) {-# UNPACK #-} !(Pair Int)

getSize :: Picture -> Pair Int
getSize (Picture _ _ _ s) = s
{-# INLINE getSize #-}

getSizeM :: PrimMonad m => MPicture (PrimState m) -> m (Pair Int)
getSizeM mPic = do
  let (MPicture _ _ _ s) = mPic
  return s
{-# INLINE getSizeM #-}

mutPic :: Picture -> (MPicture s -> ST s (MPicture s)) -> Picture
mutPic pic f = unsafeInlineST $ do
  mPic <- unsafeThawPic pic
  newPic <- f mPic
  unsafeFreezePic newPic

unsafeFreezePic :: PrimMonad m => MPicture (PrimState m) -> m Picture
unsafeFreezePic (MPicture rs gs bs s) = do
  rs' <- unsafeFreezeByteArray rs
  gs' <- unsafeFreezeByteArray gs
  bs' <- unsafeFreezeByteArray bs
  return $ Picture rs' gs' bs' s
{-# INLINE unsafeFreezePic #-}

unsafeThawPic :: PrimMonad m => Picture -> m (MPicture (PrimState m))
unsafeThawPic (Picture rs gs bs s) = do
  rs' <- unsafeThawByteArray rs
  gs' <- unsafeThawByteArray gs
  bs' <- unsafeThawByteArray bs
  return $  MPicture rs' gs' bs' s
{-# INLINE unsafeThawPic #-}

encode :: Pair Int -> Pair Int -> Int
{-# INLINE encode #-}
encode (Pair rs _) (Pair i j) = (i-1)*rs + (j-1)

decode :: Pair Int -> Int -> Pair Int
{-# INLINE decode #-}
decode (Pair rs _) k = Pair r q
 where
  (q,r) = quotRem k rs

inRange :: Pair Int -> Pair Int -> Bool
{-# INLINE inRange #-}
Pair r c `inRange` Pair rs cs = r <= rs &&
                                c <= cs &&
                                r > 0  &&
                                c > 0

unsafeIndexM, indexM :: PrimMonad m => MPicture (PrimState m) -> Pair Int -> m Color

(MPicture rs gs bs maxP) `unsafeIndexM` point = do
  let i = encode maxP point
  r <- readByteArray rs i
  g <- readByteArray gs i
  b <- readByteArray bs i
  return $ Triple r g b
{-# INLINE unsafeIndexM #-}

pic@(MPicture _ _ _ s) `indexM` p | p `inRange` s = pic `unsafeIndexM` p
                                  | otherwise     = error "Index out of bounds accessing Picture"

unsafeIndex, (!) :: Picture -> Pair Int -> Color

(Picture rs bs gs s) `unsafeIndex` p = Triple r g b
  where
    i = encode s p
    r = indexByteArray rs i
    g = indexByteArray gs i
    b = indexByteArray bs i
{-# INLINE unsafeIndex #-}

(!) pic p | p `inRange` getSize pic = pic `unsafeIndex` p
          | otherwise               = error "Index out of bounds accessing Picture"
