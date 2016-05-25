{-# LANGUAGE BangPatterns #-}

module Data.Picture.Gen (
    solidPic,
    blankPic,
    mathPic,
  ) where

import Data.Picture.Picture
import Data.Pair
import Data.Color

import Data.Primitive.ByteArray
import Control.Monad.ST
import Control.Loop

solidPic :: Color -> Pair Int -> Picture
{-# INLINE solidPic #-}
solidPic (Triple r g b) (Pair x y) = runST $ do
  rs <- newByteArray l
  gs <- newByteArray l
  bs <- newByteArray l
  fill rs r
  fill gs g
  fill bs b
  rs' <- unsafeFreezeByteArray rs
  gs' <- unsafeFreezeByteArray gs
  bs' <- unsafeFreezeByteArray bs
  return $ Picture rs' gs' bs' $ Pair x y
  where
    l = x * y
    fill ar c = fillByteArray ar 0 l c

-- |Create a completely white 'Picture'
blankPic :: Pair Int -- ^The size of the 'Picture'
         -> Picture
{-# INLINE blankPic #-}
blankPic = solidPic black

-- |Create a picture that generates the RGB values for each 'Point' from three different functions
mathPic :: Triple (Int -> Int -> ColorVal) -- ^The three functions to produce the RGB values
        -> Pair Int                   -- ^The size of the 'Picture'
        -> Picture
{-# INLINE mathPic #-}
mathPic (Triple fr fg fb) (Pair x y) = runST $ do
  rs <- newByteArray l
  gs <- newByteArray l
  bs <- newByteArray l
  numLoop 0 (x - 1) $ \xi ->
    numLoop 0 (y - 1) $ \yi -> do
                          let i = encode (Pair x y) (Pair xi yi)
                          writeByteArray rs i $ fr xi yi
                          writeByteArray gs i $ fg xi yi
                          writeByteArray bs i $ fb xi yi
  rs' <- unsafeFreezeByteArray rs
  bs' <- unsafeFreezeByteArray gs
  gs' <- unsafeFreezeByteArray bs
  return $ Picture rs' gs' bs' $ Pair x y
  where
    l = x * y
