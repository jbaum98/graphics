module Pbm (
  writePbm,
  module Picture, module Line,
  Pair(..), Triple(..)
  )
where

import Picture
import Line
import Data.Sequence
import Data.ByteString.Builder
import Data.Monoid
import System.IO

writePbm :: FilePath -> Picture -> IO ()
writePbm path pic = clearFile >> writeContents
  where writeContents = do
          h <- openFile path WriteMode
          hPutBuilder h $ fileContents pic
          hClose h
        clearFile = writeFile path ""

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
