{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Picture (
  Picture, Point, Color,
  Pair(..), Triple(..),
  size,
  -- * Creation
  blankPic, mathPic,
  -- * Manipulation
  setPointColor, setColors, setColor, transformOrigin
  ) where

import Color
import Point
import Data.Sequence hiding (zip)
import Prelude hiding (head, length, replicate)

-- |A picture is a two-dimensional 'Seq' of pixels
type Picture = Seq (Seq Color)

-- |Compute the size of a 'Picture'
size :: Picture ->
       Point -- ^A point representing the size of x and y dimensions as its
             -- x and y coordinates. The point itself is out of bounds, similar
             -- to how the length of a list as an index is out of bounds.
size = (Pair xres yres <*>) . pure
  where xres = length . head
        yres = length
        head = flip index 1

-- |Create a completely white 'Picture'
blankPic :: Point -- ^The size of the 'Picture'
           -> Picture
blankPic (Pair xr yr) = replicate yr oneRow
  where oneRow = replicate xr white

-- |Create a picture that generates the RGB values for each 'Point'
-- from three different functions
mathPic :: Triple (Point -> ColorVal) -- ^The three functions to produce the RGB values
          -> Point                   -- ^The size of the 'Picture'
          -> Picture
mathPic funcs (Pair xr yr) =
  fromList [fromList [ genColor (Pair x y) | x <- [0..xr]] | y <- [0..yr] ]
  where genColor = (cappedFuncs <*>) . pure
        cappedFuncs = fmap ((`mod` maxColor).) funcs

-- |Set the value of a single 'Point' in a 'Picture' to a given 'Color'
setPointColor :: Color -> Point -> Picture -> Picture
setPointColor pixel (Pair x y) pic = update y newRow pic
  where newRow = update x pixel oldRow
        oldRow = index pic y

-- |Set the value of a every 'Point' in a list
-- to the corresponding 'Color' at the corresponding position in the '[Color]'.
setColors :: [Color] -> [Point] -> Picture -> Picture
setColors pixels points = foldl (.) id setAllColors
  where setAllColors = map (uncurry setPointColor) pairs
        pairs = zip pixels points

-- |Set every 'Point' in a list to a single 'Color'
setColor :: Color -> [Point] -> Picture -> Picture
setColor color = setColors (repeat color)

-- |transform a 'Point' so that a given 'Point'is the origin
-- instead of the top-left corner
transformOrigin :: Point -- ^The new origin
                  -> Point -- ^The 'Point' to be transformed
                  -> Point
transformOrigin o = translate o . reflect
  where reflect = (<*>) $ Pair id negate
