{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Picture (
  Picture, Point, Pixel,
  Pair(..), Triple(..),
  size,
  -- * Creation
  blankPic, mathPic,
  -- * Manipulation
  setPixel, setPixels, setColor, transformOrigin
  ) where

import Pixel
import Point
import Data.Sequence hiding (zip)
import Prelude hiding (head, length, replicate)

-- |A picture is a two-dimensional 'Seq' of pixels
type Picture = Seq (Seq Pixel)

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
  fromList [fromList [ genPixel (Pair x y) | x <- [0..xr]] | y <- [0..yr] ]
  where genPixel = (cappedFuncs <*>) . pure
        cappedFuncs = fmap ((`mod` maxPixel).) funcs

-- |Set the value of a single 'Point' in a 'Picture' to a given 'Pixel'
setPixel :: Pixel -> Point -> Picture -> Picture
setPixel pixel (Pair x y) pic = update y newRow pic
  where newRow = update x pixel oldRow
        oldRow = index pic y

-- |Set the value of a every 'Point' in a list
-- to the corresponding 'Pixel' at the corresponding position in the '[Pixel]'.
setPixels :: [Pixel] -> [Point] -> Picture -> Picture
setPixels pixels points = foldl (.) id setAllPixels
  where setAllPixels = map (uncurry setPixel) pairs
        pairs = zip pixels points

-- |Set every 'Point' in a list to a single 'Pixel'
setColor :: Pixel -> [Point] -> Picture -> Picture
setColor color = setPixels (repeat color)

-- |transform a 'Point' so that a given 'Point'is the origin
-- instead of the top-left corner
transformOrigin :: Point -- ^The new origin
                  -> Point -- ^The 'Point' to be transformed
                  -> Point
transformOrigin o = translate o . reflect
  where reflect = (<*>) $ Pair id negate
