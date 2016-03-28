module Matrix.D3Point (
  D3Coord, D3Point,
  Triple(..)
  ) where

import Pair

-- |Represents a single coordinate of a point in 3D space
type D3Coord = Double

-- |Represents a point in 3D space
type D3Point = Triple D3Coord
