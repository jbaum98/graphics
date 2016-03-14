{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Picture (
    module Color,
    module Point,
    module Line,
    Picture,
    solidPic,
    blankPic,
    size,
    (!),
    setPointColor,
    setColor,
    transformOrigin,
    centerPoint,
    drawColorLine,
    drawLine,
    ) where

import           Color
import           Point
import           Line
import           Utils (compose)
import           Data.Array hiding ((!))
import           Data.Array.MArray
import           Data.Array.Unsafe
import           Data.Array.ST
import qualified Data.Array as A ((!))
import           Control.Monad.Primitive
import           Control.Monad.ST

-- |A 'Picture' is a grid of pixels
type Picture = Array (Coord, Coord) Color

-- |Create a solid 'Picture' of a single 'Color
solidPic :: Color
         -> Point -- ^The size of the 'Picture'
         -> Picture
solidPic color maxPoint = listArray (toSize maxPoint) . repeat $ color

-- |Create a completely white 'Picture'
blankPic :: Point -- ^The size of the 'Picture'
         -> Picture
blankPic = solidPic white

-- |Create a picture that generates the RGB values for each 'Point' from three different functions
mathPic :: Triple (Point -> ColorVal) -- ^The three functions to produce the RGB values
        -> Point                   -- ^The size of the 'Picture'
        -> Picture
mathPic funcs maxPoint = listArray (toSize maxPoint) $ map pointToColor (allPoints maxPoint)
  where
    allPoints (Pair xr yr) = [ Pair x y
                             | x <- [0 .. xr - 1]
                             , y <- [0 .. yr - 1] ]
    pointToColor p = fmap ($p) cappedFuncs
    cappedFuncs :: Triple (Point -> ColorVal)
    cappedFuncs = ((`mod` maxColor) .) <$> funcs

-- |Compute the size of a 'Picture'
size :: Picture
     -> Point -- ^A point representing the size of x and y dimensions as its
-- x and y coordinates. The point itself is out of bounds, similar to how the length of a list as an
-- index is out of bounds.
size pic = Pair xsize ysize
  where
    (_, (xsize, ysize)) = bounds pic

(!) :: Picture -> Point -> Color
pic !point = pic A.! (toTup point)

fromTup :: (a, a) -> Pair a
fromTup (x, y) = Pair x y

toTup :: Pair a -> (a, a)
toTup (Pair x y) = (x, y)

toSize :: Pair Coord -> ((Coord, Coord), (Coord, Coord))
toSize point = ((0, 0), toTup point)

inBounds :: Point -> Picture -> Bool
inBounds point pic = inRange (bounds pic) (toTup point)

-- |Set the value of a single 'Point' in a 'Picture' to a given 'Color'
setPointColor :: Color -> Point -> Picture -> Picture
setPointColor _ point pic
  | not $ inBounds point pic = pic
setPointColor color point pic = unsafeInlineST $ do
  mutPic <- unsafeThaw pic :: ST s (STArray s (Coord, Coord) Color)
  writeArray mutPic (toTup point) color
  unsafeFreeze mutPic

-- |Set every 'Point' in a list to a single 'Color'
setColor :: Color -> [Point] -> Picture -> Picture
setColor color points = compose (fmap (setPointColor color) points)

-- |transform a 'Point' so that a given 'Point'is the origin instead of the top-left corner
transformOrigin :: Point -- ^The new origin
                -> Point -- ^The 'Point' to be transformed
                -> Point
transformOrigin o = translate o . reflect
  where
    reflect = (<*>) $ Pair id negate

centerPoint :: Picture -> Point
centerPoint = fmap (round . half . fromIntegral) . size
  where
    half = (/ (2 :: Double))

drawColorLine :: Color -> Point -> Point -> Picture -> Picture
drawColorLine color p1 p2 pic = setColor color (line p1' p2') $ pic
  where
    Pair p1' p2' = (transformOrigin $ centerPoint pic) <$> Pair p1 p2

drawLine :: Point -> Point -> Picture -> Picture
drawLine = drawColorLine black
