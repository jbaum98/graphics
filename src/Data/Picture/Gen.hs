{-# LANGUAGE BangPatterns #-}

module Data.Picture.Gen (
    solidPic,
    blankPic,
    mathPic,
  ) where

import Data.Picture.Picture
import Data.Pair
import Data.Color
import Data.Vector.Unboxed.Mutable
import Data.Vector.Unboxed (generate, unsafeThaw)
import Prelude hiding (replicate)

import Control.Monad.Primitive

solidPic :: PrimMonad m => Color -> Pair Int -> m (Picture (PrimState m))
{-# INLINE solidPic #-}
solidPic color (Pair x y) = flip Picture (Pair x y) <$> replicate  (x*y) color

-- |Create a completely white 'Picture s'
blankPic :: PrimMonad m => Pair Int -- ^The size of the 'Picture s'
         -> m (Picture (PrimState m))
{-# INLINE blankPic #-}
blankPic = solidPic black

-- |Create a picture that generates the RGB values for each 'Point' from three different functions
mathPic :: PrimMonad m => Triple (Int -> Int -> ColorVal) -- ^The three functions to produce the RGB values
        -> Pair Int                   -- ^The size of the 'Picture s'
        -> m (Picture (PrimState m))
{-# INLINE mathPic #-}
mathPic (Triple fr fg fb) (Pair x y) = flip Picture (Pair x y) <$> vec
  where
    vec = unsafeThaw $ generate (x*y) $ \i -> case decode (Pair x y) i of Pair x' y' -> Triple (fr x' y') (fg x' y') (fb x' y')
