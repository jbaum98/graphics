{-|
Module      : D2Point
Description : D2Point in the xy-plane type

The 'D2Point' type represents an point in the xy-plane.
-}
module Data.D2Point (
  D2Coord, D2Point,
  Pair(..),
  translate,
  -- * Getters
  getX, getY
  ) where

import Data.Pair

-- |The type of a single coordinate
type D2Coord = Int

-- |A point in the xy-plane
type D2Point = Pair D2Coord

-- |Get the x-value of a 'D2Point'
getX :: Pair a -> a
getX (Pair x _) = x
-- |Get the y-value of a 'D2Point'
getY :: Pair a -> a
getY (Pair _ y) = y

-- |Translates a point by another point
translate :: D2Point -- ^The 'D2Point' by which to translate, for example
                  -- @translate (Pair 1 1)@ would translate up 1 and right 1
          -> D2Point -- ^The 'D2Point' to be translated
          -> D2Point -- ^The resulting point
translate = (+)
