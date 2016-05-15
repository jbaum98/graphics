{-# LANGUAGE TupleSections #-}

{-|
Module      : Pbm
Description : NetPBM format

Write a 'Picture' to a file in the NetPBM format.
-}
module Pbm (writePbmFile, writePbm) where

import Picture
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Builder.Prim
import Data.Monoid
import System.IO

-- |Write a 'Picture' to a 'FilePath' in the NetPBM format
writePbmFile :: FilePath -> Picture -> IO ()
writePbmFile path pic = withFile path WriteMode $ writePbm pic

-- |Write a 'Picture' to a 'Handle' in the NetPBM format
writePbm :: Picture -> Handle -> IO ()
writePbm pic h = do
  hSetBinaryMode h True
  hSetBuffering h $ BlockBuffering (Just 4096)
  let w = hPutBuilder h
      {-# INLINE w #-}
      Pair xres yres =  size pic
      c' = liftFixedToBounded char7
      {-# INLINE c' #-}
      is = (,' ') >$< word8Dec >*< c'
      {-# INLINE is #-}
      trip = is >*< is >*< is
      {-# INLINE trip #-}
      p3 = (\(x,y,m) -> ('P',('3',(' ',(x,(' ',(y,(' ', (m,'\n'))))))))) >$< c' >*< c' >*< c' >*< intDec >*< c'>*< intDec >*< c' >*< word8Dec >*< c'
      {-# INLINE p3 #-}
      encodeP x y = (pic `unsafeAt` (x,y,0), (pic `unsafeAt` (x,y,1), pic `unsafeAt` (x,y,2)))
      {-# INLINE encodeP #-}
  w $ primBounded p3 (xres, yres, maxColor)
  w $ primMapListBounded trip [encodeP x y | x <- [1..xres], y <- [1..yres]]
