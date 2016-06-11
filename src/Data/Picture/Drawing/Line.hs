{-# LANGUAGE BangPatterns, TupleSections #-}

{-|
Module      : Line
Description : Draw lines between two 'Int -> Int's

Provides an implementation of Bresenham's Line Algorithm. -}
module Data.Picture.Drawing.Line (
  line,
  writeLine,
  drawColorLine,
  drawLine
  ) where

import Control.Monad

import Control.Loop

import Data.Color
import Data.Pair
import Data.Picture.Picture
import Data.Picture.Drawing.Points
import Control.Monad.Primitive

data Octant = First | Second | Third   | Fourth |
              Fifth | Sixth  | Seventh | Eighth

line :: PrimMonad m => Int -> Int -> Double -> Color -> Int -> Int -> Double -> Color -> Picture (PrimState m)-> m ()
line !x1 !y1 !z1 !c1 !x2 !y2 !z2 !c2 !mpic = writeLine x1 y1 z1 (fromIntegral <$> c1) x2 y2 z2 (fromIntegral <$> c2) mpic
  where
    Pair !x1' !y1' = reflect yMax $ Pair x1 y1
    Pair !x2' !y2' = reflect yMax $ Pair x2 y2
    Pair _ yMax = getSize mpic

-- |Draw a line between two 'Int -> Int's
writeLine :: PrimMonad m
          => Int -> Int -> Double -> Triple Double
          -> Int -> Int -> Double -> Triple Double
          -> Picture (PrimState m) -> m ()
writeLine !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 = writeLine' oct x1 y1 z1 c1 x2 y2 z2 c2
  where oct = if x2 > x1
              then if y2 > y1 -- Right Half
                   then if dx > dy -- Q1
                        then First
                        else Second
                   else if dx > -dy -- Q4
                        then Eighth
                        else Seventh
              else if y2 > y1 -- Left Half
                   then if -dx > dy -- Q2
                        then Fourth
                        else Third
                   else if -dx > -dy -- Q3
                        then Fifth
                        else Sixth
        dx = x2 - x1
        dy = y2 - y1

writeLine' :: PrimMonad m => Octant -> Int -> Int -> Double -> Triple Double -> Int -> Int -> Double -> Triple Double -> Picture (PrimState m) -> m ()
writeLine' Third  !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 pic = writeLine' Seventh x2 y2 z2 c2 x1 y1 z1 c1 pic
writeLine' Fourth !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 pic = writeLine' Eighth  x2 y2 z2 c2 x1 y1 z1 c1 pic
writeLine' Fifth  !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 pic = writeLine' First   x2 y2 z2 c2 x1 y1 z1 c1 pic
writeLine' Sixth  !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 pic = writeLine' Second  x2 y2 z2 c2 x1 y1 z1 c1 pic

writeLine' First !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 pic =
  void $ forLoopState x1 (<= x2) (+1) (y1,z1,c1,di) $ \(y,z,c,d) x -> do
    writePoint (round <$> c) x y z pic
    return . uncurry ( ,z+dz,c+dc, ) $ if d < 0 then (y,d+a+a) else (y+1,d+a+a+b+b)
  where a = y2 - y1
        b = x1 - x2
        di = a + a + b
        dz = (z2 - z1) / fromIntegral (x2 - x1)
        dc = (c2 - c1) / pure (fromIntegral $ x2 - x1)

writeLine' Second !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 pic =
  void $ forLoopState y1 (<= y2) (+1) (x1,z1,c1,di) $ \(x,z,c,d) y -> do
    writePoint (round <$> c) x y z pic
    return . uncurry ( ,z+dz,c+dc, ) $ if d > 0 then (x,d+b+b) else (x+1,b+b)
  where a = y2 - y1
        b = x1 - x2
        di = a + b + b
        dz = (z2 - z1) / fromIntegral (y2 - y1)
        dc = (c2 - c1) / pure (fromIntegral $ y2 - y1)

writeLine' Eighth !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 pic =
  void $ forLoopState x1 (<= x2) (+1) (y1,z1,c1, di) $ \(y,z,c,d) x -> do
    writePoint (round <$> c) x y z pic
    return . uncurry ( ,z+dz,c+dc, ) $ if d > 0 then (y,d+a+a) else (y-1,d+a+a-b-b)
  where a = y2 - y1
        b = x1 - x2
        di = a + a - b
        dz = (z2 - z1) / fromIntegral (x2 - x1)
        dc = (c2 - c1) / pure (fromIntegral $ x2 - x1)

writeLine' Seventh !x1 !y1 !z1 c1 !x2 !y2 !z2 c2 pic =
  void $ forLoopState y1 (>= y2) (\n -> n-1) (x1,z1,c1,di) $ \(x,z,c,d) y -> do
    writePoint (round <$> c) x y z pic
    return . uncurry ( ,z+dz,c+dc, ) $ if d < 0 then (x,d-b-b) else (x+1,d+a+a-b-b)
  where a = y2 - y1
        b = x1 - x2
        di = a - b - b
        dz = (z2 - z1) / fromIntegral (y2 - y1)
        dc = (c2 - c1) / pure (fromIntegral $ y2 - y1)

drawColorLine :: PrimMonad m => Color -> Int -> Int -> Double -> Int -> Int -> Double -> Picture (PrimState m) -> m ()
drawColorLine color !x1 !y1 !z1 !x2 !y2 !z2 = line x1 y1 z1 color x2 y2 z2 color

drawLine :: PrimMonad m => Int -> Int -> Double -> Int -> Int -> Double -> Picture (PrimState m) -> m ()
drawLine = drawColorLine white
