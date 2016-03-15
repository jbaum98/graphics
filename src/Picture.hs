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
    mathPic,
    size,
    (!), elems,
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
import           Data.Array.Unboxed
import           Data.Array.MArray
import           Data.Array.Unsafe
import           Data.Array.ST
import           Control.Monad.Primitive
import           Control.Monad.ST

-- |A 'Picture' is a grid of pixels
type Picture = UArray (Coord, Coord, Int) ColorVal

-- |Create a solid 'Picture' of a single 'Color
solidPic :: Color
         -> Point -- ^The size of the 'Picture'
         -> Picture
solidPic color maxPoint = listArray (toSize maxPoint) . cycleColor $ color
  where
    cycleColor = cycle . toList
    toList (Triple r g b) = [r, g, b]

-- |Create a completely white 'Picture'
blankPic :: Point -- ^The size of the 'Picture'
         -> Picture
blankPic = solidPic white

-- |Create a picture that generates the RGB values for each 'Point' from three different functions
mathPic :: Triple (Coord -> Coord -> ColorVal) -- ^The three functions to produce the RGB values
        -> Point                   -- ^The size of the 'Picture'
        -> Picture
mathPic funcs maxPoint = listArray (toSize maxPoint) (allPoints maxPoint)
  where
    allPoints :: Point -> [ColorVal]
    allPoints (Pair xr yr) = [ colorFunc x y ci
                             | x <- [0 .. xr - 1]
                             , y <- [0 .. yr - 1]
                             , ci <- [0 .. 2] ]
    colorFunc :: Coord -> Coord -> Int -> ColorVal
    colorFunc x y ci = colorValFunc x y `mod` maxColor
      where
        colorValFunc = funcs `tIndex` ci

tIndex :: Triple a -> Int -> a
(Triple x _ _) `tIndex` 0 = x
(Triple _ x _) `tIndex` 1 = x
(Triple _ _ x) `tIndex` 2 = x
_ `tIndex` _ = error "Tried to access an index greater than 2 of a Triple"

-- |Compute the size of a 'Picture'
size :: Picture
     -> Point -- ^A point representing the size of x and y dimensions as its
-- x and y coordinates. The point itself is out of bounds, similar to how the length of a list as an
-- index is out of bounds.
size pic = Pair xsize ysize
  where
    (_, (xsize, ysize, _)) = bounds pic

toTup :: Pair a -> Int -> (a, a, Int)
toTup (Pair x y) ci = (x, y, ci)

toSize :: Pair Coord -> ((Coord, Coord, Coord), (Coord, Coord, Coord))
toSize point = ((0, 0, 0), toTup point 3)

inBounds :: Point -> Picture -> Bool
inBounds point pic = inRange (bounds pic) (toTup point 0)

-- |Set the value of a single 'Point' in a 'Picture' to a given 'Color'
setPointColor :: Color -> Point -> Picture -> Picture
setPointColor _ point pic
  | not $ inBounds point pic = pic
setPointColor color point pic = unsafeInlineST $ do
  mutPic <- unsafeThaw pic :: ST s (STUArray s (Coord, Coord, Int) ColorVal)
  writeArray mutPic (toTup point 0) (color `tIndex` 0)
  writeArray mutPic (toTup point 1) (color `tIndex` 1)
  writeArray mutPic (toTup point 2) (color `tIndex` 2)
  unsafeFreeze mutPic
  {-where
    writeColorVal p ci = writeArray p (toTup point ci) (color `tIndex` ci)-}

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
