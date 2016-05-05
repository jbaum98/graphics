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
import Control.Loop

-- |Write a 'Picture' to a 'FilePath' in the NetPBM format
writePbmFile :: FilePath -> Picture -> IO ()
writePbmFile path pic = withFile path WriteMode $ writePbm pic

-- |Write a 'Picture' to a 'Handle' in the NetPBM format
writePbm :: Picture -> Handle -> IO ()
writePbm pic h = do
  hSetBinaryMode h True
  hSetBuffering h $ BlockBuffering Nothing
  let w = hPutBuilder h
      {-# INLINE w #-}
      Pair xres yres =  size pic
  w $ string7 "P3 " <> intDec xres <> char7 ' ' <> intDec yres <> char7 ' ' <> word8Dec maxColor <> char7 '\n'
  numLoop 1 xres $ \x -> do
    numLoop 1 yres $ \y ->
      w $ word8Dec (pic ! (x,y,0)) <> char7 ' '
       <> word8Dec (pic ! (x,y,1)) <> char7 ' '
       <> word8Dec (pic ! (x,y,2)) <> char7 ' '
    w $ char7 '\n'
