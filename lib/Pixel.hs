module Pixel (
  ColorVal, Pixel, Triple(..),
  maxPixel,
  getRed, getGreen, getBlue,
  black, white
  ) where

import Pair

type ColorVal = Integer
type Pixel = Triple ColorVal

maxPixel :: ColorVal
maxPixel = 256

getRed   :: Pixel -> ColorVal
getRed   (Triple r _ _) = r

getGreen :: Pixel -> ColorVal
getGreen (Triple _ g _) = g

getBlue  :: Pixel -> ColorVal
getBlue  (Triple _ _ b) = b

black :: Pixel
black = Triple 0 0 0

white :: Pixel
white = Triple maxPixel maxPixel maxPixel
