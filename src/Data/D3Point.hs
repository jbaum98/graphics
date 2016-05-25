module Data.D3Point (
  D3Coord, D3Point,
  Triple(..),
  getX,
  getY,
  getZ
  ) where

import Data.Pair

-- |Represents a single coordinate of a point in 3D space
type D3Coord = Double

-- |Represents a point in 3D space
type D3Point = Triple D3Coord


getX, getY, getZ :: Triple a -> a
getX (Triple x _ _) = x
getY (Triple _ y _) = y
getZ (Triple _ _ z) = z
