module Pbm (
  writePbm,
  blankPic, mathPic,
  red, green, blue,
  Picture, Pixel, Point,
  Pair(..), Triple(..)
  )
where

import Picture
import State
import Control.Applicative ((<$>))
import qualified Data.Vector as V

writePbm :: FilePath -> Picture -> IO ()
writePbm path pic = foldl (>>) clearFile $ map (appendFileLine path) pieces
  where pieces = ["P3", xresStr, yresStr, show maxPixel, pixels]
        Pair xresStr yresStr = show <$> size pic
        pixels = pixelsStr pic
        appendFileLine file s = appendFile file s >> appendFile file "\n"
        clearFile = writeFile path ""

pixelsStr :: Picture -> String
pixelsStr = unlinesV . fmap rowStr
  where rowStr = unwordsV . fmap pixelStr

pixelStr :: Pixel -> String
pixelStr = unwordsV . fmap show . colorList
  where colorList =  apF $ V.fromList [red, green, blue]

unwordsV :: V.Vector String -> String
unwordsV v = case V.length v of
  0 ->  ""
  _ -> V.foldr1 (\w s -> w ++ ' ':s) v

unlinesV :: V.Vector String -> String
unlinesV v = case V.length v of
  0 ->  ""
  _ -> V.foldr1 (\w s -> w ++ '\n':s) v
