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
import Data.Foldable
import Data.Monoid
import Control.Applicative ((<$>))
import System.IO

writePbm :: FilePath -> Picture -> IO ()
writePbm path pic = clearFile >> writeContents
  where writeContents = do
          h <- openFile path WriteMode
          hPutBuilder h fileContents
        fileContents = join (char7 ' ') id pieces <> pixels
        pieces = [string7 "P3", xresStr, yresStr, intDec maxPixel]
        Pair xresStr yresStr = intDec <$> size pic
        pixels = renderPic pic
        clearFile = writeFile path ""

join :: (Foldable f, Monoid m) => m -> (a -> m) -> f a -> m
join c func = foldMap (\x -> func x <> c)

renderPic :: Picture -> Builder
renderPic = join (char7 '\n') renderRow

renderRow :: Seq Pixel -> Builder
renderRow = foldMap renderPixel

renderPixel :: Pixel -> Builder
renderPixel = join (char7 ' ') intDec
