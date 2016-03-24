module Matrix.D3Point (
  D3Coord, D3Point,
  Triple(..)
  ) where

import Pair

type D3Coord = Double
type D3Point = Triple D3Coord
