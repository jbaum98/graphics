{-# LANGUAGE MagicHash, UnboxedTuples, LambdaCase, RankNTypes, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Data.Picture.Picture (
    Picture(Picture),
    getSize,
    encode, decode, inRange,
    indexM
    ) where

import Control.Monad.Primitive
import Data.Primitive.ByteArray

import Data.Color
import Data.Pair

-- A 'Picture' stores the RGB values of each pixel
data Picture s = Picture {
  -- | An array to store the red value of each pixel as a Word8
    _rs :: !(MutableByteArray s)
  -- | An array to store the green value of each pixel as a 'Word8'
  , _gs :: !(MutableByteArray s)
  -- | An array to store the blue value of each pixel as a 'Word8'
  , _bs :: !(MutableByteArray s)
  -- | An zbuffer array storing the z value of each pixel as a 'Double', therefore
  -- this must be larger than the color array
  , _zs :: !(MutableByteArray s)
  , _size :: !(Pair Int)          -- ^ The size as @Pair xMax yMax@
  }

getSize :: Picture s -> Pair Int
getSize (Picture _ _ _ _ s) = s
{-# INLINE getSize #-}

-- | Convert a @Pair Int@ into the corresponding index for that point in the
-- color arrays
encode :: Pair Int -- ^ The size of the 'Picture'
       -> Pair Int -- ^ The pixel location
       -> Int
encode (Pair rs _) (Pair i j) = i*rs + j
{-# INLINE encode #-}

-- | Convert an index back into the pixel coordinates
decode :: Pair Int -- ^ The size of the 'Picture'
       -> Int      -- ^ The index of the array
       -> Pair Int
decode (Pair rs _) k = Pair r q
 where
  (q,r) = quotRem k rs
{-# INLINE decode #-}

-- | Determines if a pixel location is within the bounds of a 'Picture'
inRange :: Pair Int -- ^ The size of the 'Picture'
        -> Pair Int -- ^ The pixel location
        -> Bool
Pair r c `inRange` Pair rs cs = r <= rs &&
                                c <= cs &&
                                r > 0  &&
                                c > 0
{-# INLINE inRange #-}

-- | Access the 'Color' value of a pixel
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
