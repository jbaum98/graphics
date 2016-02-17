module Point (
  Coord, Point, Pair(..),
  uncurryPair,
  translate
  ) where

import Pair
import Control.Applicative

type Coord = Integer
type Point = Pair Coord

translate :: Point -> Point -> Point
translate by p = (+) <$> by <*> p
