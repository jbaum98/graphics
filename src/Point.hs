module Point (
  Coord, Point, Pair(..),
  uncurryPair,
  translate
  ) where

import Pair

type Coord = Int
type Point = Pair Coord

translate :: Point -> Point -> Point
translate by p = (+) <$> by <*> p
