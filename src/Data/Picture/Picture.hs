{-# LANGUAGE MagicHash, UnboxedTuples, LambdaCase, RankNTypes, BangPatterns #-}

{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Data.Picture.Picture (
    Picture(..),
    getSize,
    encode, decode, inRange,
    indexM
    ) where

import Control.Monad.Primitive
import Data.Primitive.ByteArray

import Data.Color
import Data.Pair

data Picture s = Picture {-# UNPACK #-} !(MutableByteArray s) {-# UNPACK #-} !(MutableByteArray s) {-# UNPACK #-} !(MutableByteArray s) {-# UNPACK #-} !(MutableByteArray s) {-# UNPACK #-} !(Pair Int)

getSize :: Picture s -> Pair Int
getSize (Picture _ _ _ _ s) = s
{-# INLINE getSize #-}

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

unsafeIndexM, indexM :: PrimMonad m => Picture (PrimState m) -> Pair Int -> m Color

(Picture rs gs bs _ maxP) `unsafeIndexM` point = do
  let i = encode maxP point
  r <- readByteArray rs i
  g <- readByteArray gs i
  b <- readByteArray bs i
  return $ Triple r g b
{-# INLINE unsafeIndexM #-}

pic@(Picture _ _ _ _ s) `indexM` p | p `inRange` s = pic `unsafeIndexM` p
                                   | otherwise     = error "Index out of bounds accessing Picture"
