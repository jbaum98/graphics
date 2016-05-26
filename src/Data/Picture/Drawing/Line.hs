{-# LANGUAGE BangPatterns #-}

{-|
Module      : Line
Description : Draw lines between two 'Int -> Int's

Provides an implementation of Bresenham's Line Algorithm.
-}
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

line :: PrimMonad m => Int -> Int -> Int -> Int -> Color -> Picture (PrimState m)-> m ()
line !x1 !y1 !x2 !y2 !color !mpic = writeLine x1' y1' x2' y2' color mpic
  where
    Pair !x1' !y1' = reflect yMax $ Pair x1 y1
    Pair !x2' !y2' = reflect yMax $ Pair x2 y2
    Pair _ yMax = getSize mpic

-- |Draw a line between two 'Int -> Int's
writeLine :: PrimMonad m => Int -> Int -- ^Starting point
          -> Int -> Int -- ^Ending point
          -> Color
          -> Picture (PrimState m) -> m ()
writeLine !x1 !y1 !x2 !y2 color = writeLine' oct x1 y1 x2 y2 color
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

writeLine' :: PrimMonad m => Octant -> Int -> Int -> Int -> Int -> Color -> Picture (PrimState m) -> m ()
writeLine' Third  !x1 !y1 !x2 !y2 color pic = writeLine' Seventh x2 y2 x1 y1 color pic
writeLine' Fourth !x1 !y1 !x2 !y2 color pic = writeLine' Eighth  x2 y2 x1 y1 color pic
writeLine' Fifth  !x1 !y1 !x2 !y2 color pic = writeLine' First   x2 y2 x1 y1 color pic
writeLine' Sixth  !x1 !y1 !x2 !y2 color pic = writeLine' Second  x2 y2 x1 y1 color pic

writeLine' First !x1 !y1 !x2 !y2 color pic = do
  void $ forLoopState x1 (<= x2) (+1) (y1,di) $ \(y,d) x -> do
    writePoint color x y $ pic
    return $ if d < 0 then (y,d+a+a) else (y+1,d+a+a+b+b)
  where a = y2 - y1
        b = x1 - x2
        di = a + a + b

writeLine' Second !x1 !y1 !x2 !y2 color pic = do
  void $ forLoopState y1 (<= y2) (+1) (x1,di) $ \(x,d) y -> do
    writePoint color x y pic
    return $ if d > 0 then (x,d+b+b) else (x+1,d+a+a+b+b)
  where a = y2 - y1
        b = x1 - x2
        di = a + b + b

writeLine' Eighth !x1 !y1 !x2 !y2 color pic = do
  void $ forLoopState x1 (<= x2) (+1) (y1,di) $ \(y,d) x -> do
    writePoint color x y pic
    return $ if d > 0 then (y,d+a+a) else (y-1,d+a+a-b-b)
  where a = y2 - y1
        b = x1 - x2
        di = a + a - b

writeLine' Seventh !x1 !y1 !x2 !y2 color pic = do
  void $ forLoopState y1 (>= y2) (\n -> n-1) (x1,di) $ \(x,d) y -> do
    writePoint color x y pic
    return $ if d < 0 then (x,d-b-b) else (x+1,d+a+a-b-b)
  where a = y2 - y1
        b = x1 - x2
        di = a - b - b

drawColorLine :: PrimMonad m => Color -> Int -> Int -> Int -> Int -> Picture (PrimState m) -> m ()
drawColorLine color !x1 !y1 !x2 !y2 = line x1 y1 x2 y2 color

drawLine :: PrimMonad m => Int -> Int -> Int -> Int -> Picture (PrimState m) -> m ()
drawLine = drawColorLine white
