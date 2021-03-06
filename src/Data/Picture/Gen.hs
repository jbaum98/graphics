{-# LANGUAGE MagicHash #-}

module Data.Picture.Gen (
    solidPic,
    blankPic,
    mathPic,
  ) where

import Data.Picture.Picture
import Data.Pair
import Data.Color

import Control.Monad.Primitive
import Data.Primitive.Types
import GHC.Exts
import Data.Primitive.ByteArray
import Control.Loop

-- | Generate a 'Picture' of completely one 'Color'
solidPic :: PrimMonad m => Color -> Pair Int -> m (Picture (PrimState m))
{-# INLINE solidPic #-}
solidPic (Triple r g b) (Pair x y) = do
  rs <- newByteArray l
  gs <- newByteArray l
  bs <- newByteArray l
  zs <- newByteArray lD
  fill rs r
  fill gs g
  fill bs b
  setByteArray zs 0 l negInf
  return $ Picture rs gs bs zs $ Pair x y
  where
    l = x * y
    lD = l * I# (sizeOf# (0 :: Double))
    fill ar = fillByteArray ar 0 l

-- |Create a completely black 'Picture'
blankPic :: PrimMonad m => Pair Int -- ^The size of the 'Picture'
         -> m (Picture (PrimState m))
{-# INLINE blankPic #-}
blankPic = solidPic black

-- |Create a 'Picture' that generates the RGB values for each 'Point' from three different functions
mathPic :: PrimMonad m => Triple (Int -> Int -> ColorVal) -- ^The three functions to produce the RGB values
        -> Pair Int                   -- ^The size of the 'Picture s'
        -> m (Picture (PrimState m))
{-# INLINE mathPic #-}
mathPic (Triple fr fg fb) (Pair x y) = do
  rs <- newByteArray l
  gs <- newByteArray l
  bs <- newByteArray l
  zs <- newByteArray l
  numLoop 0 (x - 1) $ \xi ->
    numLoop 0 (y - 1) $ \yi -> do
                          let i = encode (Pair x y) (Pair xi yi)
                          writeByteArray rs i $ fr xi yi
                          writeByteArray gs i $ fg xi yi
                          writeByteArray bs i $ fb xi yi
  setByteArray zs 0 l negInf
  return $ Picture rs gs bs zs $ Pair x y
  where
    l = x * y

negInf :: Double
negInf = -1 / 0
