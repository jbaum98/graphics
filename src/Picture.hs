{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Picture (
  Picture, Row, Point, Color,
  Pair(..), Triple(..),
  size,
  -- * Creation
   blankPic, mathPic,
  -- * Manipulation
   setPointColor, setColors, setColor, transformOrigin
  ) where

import Color
import Point
import Data.Vector hiding (zip, foldM)
import Data.Vector.Mutable (read, write)
import Control.Monad.Primitive
import Control.Monad
import Control.Applicative
import Prelude (($), (.), mod, negate, zip, uncurry, id, repeat, (>=), (<), (||), flip)

-- |A 'Picture' is a grid of pixels
type Row = Vector Color
type Picture = Vector Row

-- |Compute the size of a 'Picture'
size :: Picture ->
       Point -- ^A point representing the size of x and y dimensions as its
             -- x and y coordinates. The point itself is out of bounds, similar
             -- to how the length of a list as an index is out of bounds.
size = (Pair xres yres <*>) . pure
  where xres = length . head
        yres = length

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
mathPic funcs (Pair xr yr) = generate yr genRow
  where genRow y = generate xr (\x -> pointToColor $ Pair x y)
        pointToColor = (cappedFuncs <*>) . pure
        cappedFuncs = ((`mod` maxColor).) <$> funcs

-- |Set the value of a single 'Point' in a 'Picture' to a given 'Color'
setPointColor :: PrimMonad m => Color -> Point -> Picture -> m (Picture)
setPointColor _ (Pair x y) pic | x >= xLen || x < 0 || y >= yLen || y < 0 = return pic
  where Pair xLen yLen = size pic
setPointColor pixel (Pair x y) pic = do
  mutPic <- unsafeThaw pic
  oldRow <- read mutPic y
  mutOldRow <- unsafeThaw oldRow
  write mutOldRow x pixel
  newRow <- unsafeFreeze mutOldRow
  write mutPic y newRow
  unsafeFreeze mutPic

-- |Set the value of a every 'Point' in a list
-- to the corresponding 'Color' at the corresponding position in the '[Color]'.
setColors :: PrimMonad m => [Color] -> [Point] -> Picture -> m (Picture)
setColors pixels points pic = foldM (flip . uncurry $ setPointColor) pic pairs
  where pairs = zip pixels points

-- |Set every 'Point' in a list to a single 'Color'
setColor :: PrimMonad m => Color -> [Point] -> Picture -> m (Picture)
setColor color = setColors (repeat color)

-- |transform a 'Point' so that a given 'Point'is the origin
-- instead of the top-left corner
transformOrigin :: Point -- ^The new origin
                  -> Point -- ^The 'Point' to be transformed
                  -> Point
transformOrigin o = translate o . reflect
  where reflect = (<*>) $ Pair id negate
