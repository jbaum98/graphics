module Pixel (
  ColorVal, Pixel, Triple(..),
  maxPixel,
  getRed, getGreen, getBlue,
  black, white, red, orange, yellow, green, blue,
  indigo, violet, pink, turqouise
  ) where

import Pair
import Control.Applicative((<$>))

type ColorVal = Int
type Pixel = Triple ColorVal

maxPixel :: ColorVal
maxPixel = 255

getRed   :: Pixel -> ColorVal
getRed   (Triple r _ _) = r

getGreen :: Pixel -> ColorVal
getGreen (Triple _ g _) = g

getBlue  :: Pixel -> ColorVal
getBlue  (Triple _ _ b) = b

relative :: RealFrac a => a -> a -> ColorVal
relative oldMax oldVal = round $ fromIntegral maxPixel * oldVal / oldMax


rel255 :: RealFrac a => a -> ColorVal
rel255 = relative 255

black :: Pixel
black = Triple 0 0 0

white :: Pixel
white = Triple maxPixel maxPixel maxPixel

red :: Pixel
red = Triple maxPixel 0 0

orange :: Pixel
orange = rel255 <$> Triple 255 144 0

yellow :: Pixel
yellow = Triple maxPixel maxPixel 0

green :: Pixel
green = Triple 0 maxPixel 0

blue :: Pixel
blue = Triple 0 0 maxPixel

indigo :: Pixel
indigo = rel255 <$> Triple 75 0 130

violet :: Pixel
violet = rel255 <$> Triple 159 0 255

pink :: Pixel
pink = rel255 <$> Triple 255 192 203

turqouise :: Pixel
turqouise = rel255 <$> Triple 64 224 208
