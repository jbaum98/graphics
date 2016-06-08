module Data.Lighting (
  Lighting(..),
  PointLight(..)
) where

import Data.Color
import Data.Pair

data Lighting = Lighting
  { ambient :: !Color
  , lights  :: ![PointLight] }

data PointLight = PointLight
  { color :: !Color
  , loc   :: !(Triple Double) }
