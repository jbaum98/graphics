{-# LANGUAGE FlexibleContexts, ConstraintKinds, BangPatterns #-}

{-|
Module      : Picture
Description : Create and manipulate 'Picture's

Provides various methods to create and manipulate 'Picture's.
-}
module Picture (
    module Color,
    module D2Point,
    module Line,
    Picture(..),
    solidPic,
    blankPic,
    mathPic,
    size,
    (!),
    unsafeAt,
    elems,
    setPointColor,
    setColor,
    drawColorLine,
    drawLine,
    ) where

import           Control.Monad.ST
import           Data.Array.Unboxed
import           Data.Array.MArray
import           Data.Array.Unsafe
import           Data.Array.ST
import qualified Data.Array.Base as AB (unsafeAt)

import           Control.DeepSeq

import           Color
import           D2Point
import           Line
import           Utils (compose)

type Coord = D2Coord
type Point = D2Point

-- |A 'Picture' is a grid of pixels
newtype Picture = Picture { runPicture :: UArray (Coord, Coord, Int) ColorVal }

instance NFData Picture where
  rnf (Picture x) = rnf (bounds x, elems x)

-- |Create a solid 'Picture' of a single 'Color
solidPic :: Color
         -> Point -- ^The size of the 'Picture'
         -> Picture
{-# INLINE solidPic #-}
solidPic (Triple r g b) maxPoint | r == g && r == b = Picture $ runSTUArray $ newArray (toSize maxPoint) r
                                 | otherwise = Picture $ listArray (toSize maxPoint) $ cycle [r,g,b]

-- |Create a completely white 'Picture'
blankPic :: Point -- ^The size of the 'Picture'
         -> Picture
{-# INLINE blankPic #-}
blankPic maxPoint = Picture $ runSTUArray $ newArray (toSize maxPoint) 0

-- |Create a picture that generates the RGB values for each 'Point' from three different functions
mathPic :: Triple (Coord -> Coord -> ColorVal) -- ^The three functions to produce the RGB values
        -> Point                   -- ^The size of the 'Picture'
        -> Picture
{-# INLINE mathPic #-}
mathPic funcs maxPoint = Picture $ listArray (toSize maxPoint) (allPoints maxPoint)
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
    (_, (xsize, ysize, _)) = bounds $ runPicture pic

toTup :: Pair a -> Int -> (a, a, Int)
{-# INLINE toTup #-}
toTup (Pair x y) ci = (y, x, ci)

toSize :: Pair Coord -> ((Coord, Coord, Coord), (Coord, Coord, Coord))
toSize point = ((1, 1, 0), toTup point 2)

inBounds :: Point -> Picture -> Bool
inBounds point pic = inRange (bounds $ runPicture pic) (toTup point 0)

-- |Set the value of a single 'Point' in a 'Picture' to a given 'Color'
setPointColor :: Color -> Point -> Picture -> Picture
setPointColor _ point !pic
  | not $ inBounds point' pic = pic
  where point' = reflect pic point
setPointColor color point pic@(Picture arr) = runST $ do
  mutPic <- unsafeThaw arr :: ST s (STUArray s (Coord, Coord, Int) ColorVal)
  mapM_ (mutColorVal mutPic) [0..2]
  Picture <$> unsafeFreeze mutPic
  where mutColorVal mp n = writeArray mp (toTup point' n) (color `tIndex` n)
        point' = reflect pic point

reflect :: Picture -> D2Point -> D2Point
reflect pic (Pair x y) = Pair x (yMax - y - 1)
  where Pair _ yMax = size pic

-- |Set every 'Point' in a list to a single 'Color'
setColor :: Color -> [Point] -> Picture -> Picture
setColor color points = compose (fmap (setPointColor color) points)

drawColorLine :: Color -> Point -> Point -> Picture -> Picture
drawColorLine color p1 p2 = setColor color (line p1 p2)

drawLine :: Point -> Point -> Picture -> Picture
drawLine = drawColorLine white

unsafeAt :: Picture -> (Coord,Coord,Int) -> ColorVal
unsafeAt pic = AB.unsafeAt (runPicture pic) . enc
  where
      enc = index ((1,1,0),(xres,yres,2))
      {-# INLINE enc #-}
      (Pair xres yres) = size pic
{-# INLINE unsafeAt #-}
