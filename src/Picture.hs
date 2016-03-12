{-# LANGUAGE TypeOperators, FlexibleContexts, ConstraintKinds #-}

{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Picture (
  Picture, Repr, U, D, computeP, computeS, delay,
  module Point, module Color,
  size,
  -- * Creation
  blankPic, mathPic,
  -- * Access
  (!),
  -- * Manipulation
  setPointColor, setColor, transformOrigin, drawColorLine, drawLine
  ) where

import Color
import Point
import Line
import Utils (compose)
import Data.Array.Repa hiding ((!), map, traverse, size)
import qualified Data.Array.Repa as R ((!), traverse)

-- |A 'Picture' is a grid of pixels
type Ix = (Z :. Coord :. Coord :. Int)
type Picture r = Array r Ix ColorVal

type Repr r = Source r Coord

-- |Compute the size of a 'Picture'
size :: Repr r => Picture r ->
       Point -- ^A point representing the size of x and y dimensions as its
             -- x and y coordinates. The point itself is out of bounds, similar
             -- to how the length of a list as an index is out of bounds.
size pic = Pair xsize ysize
  where xsize:ysize:_ = listOfShape . extent $ pic

-- |Determine if a 'Point' is within a 'Picture'
inBounds :: Repr r => Point -> Picture r -> Bool
inBounds (Pair x y) pic = inShape (extent pic) (Z :. x :. y :. 0)

(!) :: Repr r => Picture r -> Point -> Color
pic ! (Pair x y) = Triple r g b
  where r = pic R.! (Z :. x :. y :. 0)
        g = pic R.! (Z :. x :. y :. 1)
        b = pic R.! (Z :. x :. y :. 2)

-- |Set the value of a single 'Point' in a 'Picture' to a given 'Color'
setPointColor :: Repr r => Color -> Point -> Picture r -> Picture D
setPointColor _ point pic | not $ inBounds point pic = delay pic
setPointColor color (Pair x y) pic = R.traverse pic id elemFunc
  where {-# INLINE elemFunc #-}
        elemFunc ixFunc ix@(Z :. x' :. y' :. colorIx)
          | x' == x && y == y' = getColor colorIx color
          | otherwise = ixFunc ix

getColor :: Int -> Color -> ColorVal
getColor 0 = getRed
getColor 1 = getGreen
getColor 2 = getBlue
getColor _ = error "getColor called with a number that isn't 0, 1 or 2"

setColorVal :: Color -> Int -> ColorVal
setColorVal color 0 = getRed color
setColorVal color 1 = getGreen color
setColorVal color 2 = getBlue color
setColorVal _ _ = error "setColorVal called with a number that isn't 0, 1 or 2"

toSize :: Point -> (Z :. Int :. Int :. Int)
{-# INLINE toSize #-}
toSize (Pair xr yr) = Z :. xr :. yr :. 3

shapeCurry :: (Point -> Color) -> (Ix -> ColorVal)
{-# INLINE shapeCurry #-}
shapeCurry f (Z :. x :. y :. ci) = setColorVal (f (Pair x y)) ci

-- |Create a completely white 'Picture'
blankPic :: Point -- ^The size of the 'Picture'
           -> Picture D
blankPic maxPoint = fromFunction (toSize maxPoint) (shapeCurry $ const white)

-- |Create a picture that generates the RGB values for each 'Point'
-- from three different functions
mathPic :: Triple (Point -> ColorVal) -- ^The three functions to produce the RGB values
          -> Point                   -- ^The size of the 'Picture'
          -> Picture D
mathPic funcs maxPoint = fromFunction (toSize maxPoint) (shapeCurry $ pointToColor)
  where pointToColor :: Point -> Color
        pointToColor p = fmap ($p) cappedFuncs
        cappedFuncs :: Triple (Point -> ColorVal)
        cappedFuncs = ((`mod` maxColor).) <$> funcs

-- |Set every 'Point' in a list to a single 'Color'
setColor :: Repr r => Color -> [Point] -> Picture r -> Picture D
setColor color points = compose (fmap (setPointColor color) points) . delay

-- |transform a 'Point' so that a given 'Point'is the origin
-- instead of the top-left corner
transformOrigin :: Point -- ^The new origin
                  -> Point -- ^The 'Point' to be transformed
                  -> Point
transformOrigin o = translate o . reflect
  where reflect = (<*>) $ Pair id negate

centerPoint :: Repr r => Picture r -> Point
centerPoint = fmap (round . half . fromIntegral) . size
  where half = (/ (2 :: Double))

drawColorLine :: Repr r => Color -> Point -> Point -> Picture r -> Picture D
drawColorLine color p1 p2 pic = setColor color (line p1' p2') $ pic
  where Pair p1' p2' = (transformOrigin $ centerPoint pic) <$> Pair p1 p2

drawLine :: Repr r => Point -> Point -> Picture r -> Picture D
drawLine = drawColorLine black
