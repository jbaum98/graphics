{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Data.Picture.Drawing.Points (
  writePoint,
  setPointColor,
  setColor,
  reflect
  ) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Monoid

import Data.Color
import Data.D2Point
import Data.Picture.Picture

type Coord = D2Coord
type Point = D2Point

writePoint :: Color -> Point -> STUArray s (Coord,Coord,Int) ColorVal -> ST s ()
writePoint (Triple r g b) point arr = do
  mutColorVal 0 r
  mutColorVal 1 g
  mutColorVal 2 b
  where mutColorVal n = writeArray arr (toTup point n)

-- |Set the value of a single 'Point' in a 'Picture' to a given 'Color'
setPointColor :: Color -> Point -> Picture -> Picture
setPointColor _ point !pic
  | not $ inBounds point' pic = pic
  where point' = reflect pic point
setPointColor color point pic@(Picture arr) = runST $ do
  mutArr <- unsafeThaw arr :: ST s (STUArray s (Coord, Coord, Int) ColorVal)
  writePoint color point' mutArr
  Picture <$> unsafeFreeze mutArr
  where point' = reflect pic point

inBounds :: Point -> Picture -> Bool
inBounds point pic = inRange (bounds $ runPicture pic) (toTup point 0)

reflect :: Picture -> D2Point -> D2Point
reflect pic (Pair x y) = Pair x (yMax - y - 1)
  where Pair _ yMax = size pic

-- |Set every 'Point' in a list to a single 'Color'
setColor :: Color -> [Point] -> Picture -> Picture
setColor color = appEndo . foldMap (Endo . setPointColor color)
