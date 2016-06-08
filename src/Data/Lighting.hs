module Data.Lighting (
  Lighting(..),
  PointLight(..),
  LightingConsts
) where

import Data.Color
import Data.Pair

data Lighting = Lighting
  { ambient :: !Color
  , lights  :: ![PointLight] }

data PointLight = PointLight
  { color :: !Color
  , loc   :: !(Triple Double) }

type LightingConsts = Triple (Triple Double)
