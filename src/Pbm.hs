{-|
Module      : Pbm
Description : NetPBM format

Write a 'Picture' to a file in the NetPBM format.
-}
module Pbm (writePbm) where

import Picture
import Pixel (maxPixel)
import Data.Sequence
import Data.ByteString.Builder
import Data.Monoid
import System.IO

-- |Write a 'Picture' to a 'FilePath' in the NetPBM format
writePbm :: FilePath -> Picture -> IO ()
writePbm path pic = clearFile >> writeContents
  where writeContents = do
          h <- openFile path WriteMode
          hPutBuilder h $ fileContents pic
          hClose h
        clearFile = writeFile path ""

-- |Produce the 'Builder' string representation of the file
-- for a 'Picture' including the header
fileContents :: Picture -> Builder
fileContents pic = foldMap (<> char7 ' ') pieces <> char7 '\n' <> pixels
  where pieces = [string7 "P3", xresStr, yresStr, intDec maxPixel]
        Pair xresStr yresStr = intDec <$> size pic
        pixels = renderPic pic

renderPic :: Picture -> Builder
renderPic = foldMap $ (<> char7 '\n') . renderRow

renderRow :: Seq Pixel -> Builder
renderRow = foldMap renderPixel

renderPixel :: Pixel -> Builder
renderPixel = foldMap $ (<> char7 ' ') . intDec
