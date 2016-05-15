{-# LANGUAGE BangPatterns #-}

{-|
Module      : Pbm
Description : NetPBM format

Write a 'Picture' to a file in the NetPBM format.
-}
module Pbm (writePbmFile, writePbm) where

import Picture
import Data.ByteString.Builder
import Data.Monoid
import System.IO

-- |Write a 'Picture' to a 'FilePath' in the NetPBM format
writePbmFile :: FilePath -> Picture -> IO ()
writePbmFile path pic = withFile path WriteMode $ writePbm pic

-- |Write a 'Picture' to a 'Handle' in the NetPBM format
writePbm :: Picture -> Handle -> IO ()
writePbm !pic h = do
  hSetBinaryMode h True
  hSetBuffering h $ BlockBuffering (Just 4096)
  let w = hPutBuilder h
      {-# INLINE w #-}
      Pair xres yres =  size pic
      encodeP x y = word8Dec (pic `unsafeAt` (x,y,0)) <> char7 ' '
                      <> word8Dec (pic `unsafeAt` (x,y,1)) <> char7 ' '
                      <> word8Dec (pic `unsafeAt` (x,y,2)) <> char7 ' '
  w $ string7 "P3 " <> intDec xres <> char7 ' ' <> intDec yres <> char7 ' ' <> word8Dec maxColor <> char7 '\n'
  mapM_ w [encodeP x y | x <- [1..xres], y <- [1..yres]]
