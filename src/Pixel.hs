{-|
Module      : Pixel
Description : RGB pixel type

The 'Pixel' type represents an RGB pixel.
-}
module Pixel (
  ColorVal, Pixel,
  Triple(..),
  maxPixel,
  -- * Getters
  getRed, getGreen, getBlue,
  -- * Color Constants
  black, white, red, orange, yellow, green, blue,
  indigo, violet, pink, turqouise
  ) where

import Pair (Triple(..))

-- |The type of a singe color value
type ColorVal = Int

-- |An RGB pixel consists of three numbers
-- representing the amount of red, green, and blue respectively
type Pixel = Triple ColorVal

-- |The maximum color value
maxPixel :: ColorVal
maxPixel = 255

getRed   :: Pixel -> ColorVal
getRed   (Triple r _ _) = r

getGreen :: Pixel -> ColorVal
getGreen (Triple _ g _) = g

getBlue  :: Pixel -> ColorVal
getBlue  (Triple _ _ b) = b

-- |Convert a color value from a system with a different maximum color value
-- to the system with a maximum color value of 'maxPixel'
relative :: RealFrac a
           => a -- ^The maximum color value of the original system
           -> a -- ^The color value to be converted in the original system
           -> ColorVal -- ^The converted color value for our system
relative oldMax oldVal = round $ fromIntegral maxPixel * oldVal / oldMax

-- |Convert from a 255-based color system
-- This allows us to specify colors in the very common 255-based system
-- and still switch to a new system only by changing 'maxPixel'
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
