{-|
Module      : Point
Description : Point in the xy-plane type

The 'Point' type represents an point in the xy-plane.
-}
module Point (
  Coord, Point,
  Pair(..),
  translate,
  -- * Getters
  getX, getY
  ) where

import Pair (Pair(..))

-- |The type of a single coordinate
type Coord = Int

-- |A point in the xy-plane
type Point = Pair Coord

-- |Get the x-value of a 'Point'
getX :: Pair a -> a
getX (Pair x _) = x
-- |Get the y-value of a 'Point'
getY :: Pair a -> a
getY (Pair _ y) = y

-- |Translates a point by another point
translate :: Point -- ^The 'Point' by which to translate, for example
                  -- @translate (Pair 1 1)@ would translate up 1 and right 1
          -> Point -- ^The 'Point' to be translated
          -> Point -- ^The resulting point
translate = (+)
