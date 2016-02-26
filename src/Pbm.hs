{-|
Module      : Pbm
Description : NetPBM format

Write a 'Picture' to a file in the NetPBM format.
-}
module Pbm (writePbmFile, writePbm, fileContents) where

import Picture
import Color (maxColor)
import Data.ByteString.Builder
import Data.Monoid
import System.IO

-- |Write a 'Picture' to a 'FilePath' in the NetPBM format
writePbmFile :: FilePath -> Picture -> IO ()
writePbmFile path pic = withFile path WriteMode $ writePbm pic

-- |Write a 'Picture' to a 'Handle' in the NetPBM format
writePbm :: Picture -> Handle -> IO ()
writePbm pic h = do
  hSetBinaryMode h True
  hSetBuffering h $ BlockBuffering Nothing
  hPutBuilder h $ fileContents pic

-- |Produce the 'Builder' string representation of the file
-- for a 'Picture' including the header
fileContents :: Picture -> Builder
fileContents pic = foldMap (<> char7 ' ') pieces <> char7 '\n' <> pixels
  where pieces = [string7 "P3", xresStr, yresStr, intDec maxColor]
        Pair xresStr yresStr = intDec <$> size pic
        pixels = renderPic pic

renderPic :: Picture -> Builder
{-# INLINE renderPic #-}
renderPic = foldMap $ (<> char7 '\n') . renderRow

renderRow :: Row -> Builder
{-# INLINE renderRow #-}
renderRow = foldMap renderColor

renderColor :: Color -> Builder
{-# INLINE renderColor #-}
renderColor = foldMap $ (<> char7 ' ') . intDec
