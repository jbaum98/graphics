module Pixel (
  ColorVal, Pixel, Triple(..),
  maxPixel,
  red, green, blue
  ) where

import Pair

type ColorVal = Integer
type Pixel = Triple ColorVal

maxPixel :: ColorVal
maxPixel = 256

red   :: Pixel -> ColorVal
red   (Triple r _ _) = r

green :: Pixel -> ColorVal
green (Triple _ g _) = g

blue  :: Pixel -> ColorVal
blue  (Triple _ _ b) = b
