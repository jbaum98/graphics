module Pbm (
  writePbm,
  module Picture, module Line,
  Pair(..), Triple(..)
  )
where

import Picture
import Line
import Data.Foldable
import Control.Applicative ((<$>))
import Prelude hiding (unlines, unwords, foldl, foldr)

writePbm :: FilePath -> Picture -> IO ()
writePbm path pic = foldl (>>) clearFile $ map (appendFileLine path) pieces
  where pieces = ["P3", xresStr, yresStr, show maxPixel, pixels]
        Pair xresStr yresStr = show <$> size pic
        pixels = pixelsStr pic
        appendFileLine file s = appendFile file s >> appendFile file "\n"
        clearFile = writeFile path ""

pixelsStr :: Picture -> String
pixelsStr = unlines . fmap rowStr
  where rowStr = unwords . fmap pixelStr

pixelStr :: Pixel -> String
pixelStr = unwords . fmap show . colorList
  where colorList =  apF $ fromList [getRed, getGreen, getBlue]

unChar :: Foldable f => Char -> f String -> String
unChar c = foldr (\w s -> w ++ c:s) ""

unwords :: Foldable f => f String -> String
unwords = unChar ' '

unlines :: Foldable f => f String -> String
unlines = unChar '\n'
