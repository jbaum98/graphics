module Point (Coord, Point, Pair(..)) where

import Pair

type Coord = Integer
type Point = Pair Coord
