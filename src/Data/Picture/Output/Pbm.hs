{-# LANGUAGE BangPatterns, MagicHash, TupleSections #-}

{-|
Module      : Pbm
Description : NetPBM format

Write a 'Picture' to a file in the NetPBM format.
-}
module Data.Picture.Output.Pbm (writePbm, savePbm) where

import System.IO
import Data.Primitive.ByteArray
import GHC.Word
import Control.Monad.Primitive

import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Builder.Prim

import Data.Color
import Data.Pair
import Data.Picture.Picture

-- |Write a 'Picture' to a 'FilePath' in the NetPBM format
savePbm :: FilePath -> Picture RealWorld -> IO ()
savePbm path = withFile path WriteMode . writePbm

-- |Write a 'Picture' to a 'Handle' in the NetPBM format
writePbm :: Picture RealWorld -> Handle -> IO ()
writePbm !pic h = do
  hSetBinaryMode h True
  hSetBuffering h $ BlockBuffering Nothing
  let w = hPutBuilder h
      {-# INLINE w #-}
      Pair xres yres =  getSize pic
      c' = liftFixedToBounded char7
      {-# INLINE c' #-}
      is = (,' ') >$< word8Dec >*< c'
      {-# INLINE is #-}
      trip = is >*< is >*< is
      {-# INLINE trip #-}
      p3 = (\(x,y,m) -> ('P',('3',(' ',(x,(' ',(y,(' ', (m,'\n'))))))))) >$< c' >*< c' >*< c' >*< intDec >*< c'>*< intDec >*< c' >*< word8Dec >*< c'
      {-# INLINE p3 #-}
  w $ primBounded p3 (xres, yres, maxColor)
  w $ primMapListBounded trip [indexTup pic x y | x <- [1..xres], y <- [1..yres]]

indexTup :: Picture RealWorld -> Int -> Int -> (Word8,(Word8,Word8))
indexTup (Picture rs gs bs s) x y = (r,(g,b))
  where
    i = encode s $ Pair x y
    r = indexByteArray (unsafeInlineIO $ unsafeFreezeByteArray rs) i
    g = indexByteArray (unsafeInlineIO $ unsafeFreezeByteArray gs) i
    b = indexByteArray (unsafeInlineIO $ unsafeFreezeByteArray bs) i
{-# INLINE indexTup #-}
