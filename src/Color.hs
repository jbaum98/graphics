{-|
Module      : Color
Description : RGB pixel type

The 'Color' type represents an RGB pixel.
-}
module Color (
  ColorVal, Color,
  Triple(..),
  maxColor,
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
type Color = Triple ColorVal

-- |The maximum color value
maxColor :: ColorVal
maxColor = 255

getRed   :: Color -> ColorVal
getRed   (Triple r _ _) = r

getGreen :: Color -> ColorVal
getGreen (Triple _ g _) = g

getBlue  :: Color -> ColorVal
getBlue  (Triple _ _ b) = b

-- |Convert a color value from a system with a different maximum color value
-- to the system with a maximum color value of 'maxColor'
relative :: RealFrac a
           => a -- ^The maximum color value of the original system
           -> a -- ^The color value to be converted in the original system
           -> ColorVal -- ^The converted color value for our system
relative oldMax oldVal = round $ fromIntegral maxColor * oldVal / oldMax

-- |Convert from a 255-based color system
-- This allows us to specify colors in the very common 255-based system
-- and still switch to a new system only by changing 'maxColor'
rel255 :: RealFrac a => a -> ColorVal
rel255 = relative 255

black :: Color
black = Triple 0 0 0

white :: Color
white = Triple maxColor maxColor maxColor

red :: Color
red = Triple maxColor 0 0

orange :: Color
orange = rel255 <$> Triple 255 144 0

yellow :: Color
yellow = Triple maxColor maxColor 0

green :: Color
green = Triple 0 maxColor 0

blue :: Color
blue = Triple 0 0 maxColor

indigo :: Color
indigo = rel255 <$> Triple 75 0 130

violet :: Color
violet = rel255 <$> Triple 159 0 255

pink :: Color
pink = rel255 <$> Triple 255 192 203

turqouise :: Color
turqouise = rel255 <$> Triple 64 224 208
