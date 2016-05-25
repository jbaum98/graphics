{-|
Module      : Line
Description : Draw lines between two 'Point's

Provides an implementation of Bresenham's Line Algorithm.
-}
module Data.Picture.Drawing.Line (
  line,
  writeLine,
  drawColorLine,
  drawLine
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unsafe

import Control.Loop

import Data.Color
import Data.D2Point
import Data.Picture.Picture
import Data.Picture.Drawing.Points

type Point = D2Point

data Octant = First | Second | Third   | Fourth |
              Fifth | Sixth  | Seventh | Eighth

line :: Point -> Point -> Color -> Picture -> Picture
line p1 p2 color pic = runST $ do
  mutArr <- unsafeThaw (runPicture pic)
  writeLine p1' p2' color mutArr
  Picture <$> unsafeFreeze mutArr
  where
    p1' = reflect pic p1
    p2' = reflect pic p2

-- |Draw a line between two 'Point's
writeLine :: Point -- ^Starting point
          -> Point -- ^Ending point
          -> Color
          -> STUArray s (D2Coord,D2Coord,Int) ColorVal -> ST s ()
writeLine p1@(Pair x1 y1) p2@(Pair x2 y2) color = writeLine' oct p1 p2 color
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

writeLine' :: Octant -> Point -> Point -> Color -> STUArray s (D2Coord,D2Coord,Int) ColorVal -> ST s ()
writeLine' Third  p1 p2 color pic = writeLine' Seventh p2 p1 color pic
writeLine' Fourth p1 p2 color pic = writeLine' Eighth  p2 p1 color pic
writeLine' Fifth  p1 p2 color pic = writeLine' First   p2 p1 color pic
writeLine' Sixth  p1 p2 color pic = writeLine' Second  p2 p1 color pic

writeLine' First (Pair x1 y1) (Pair x2 y2) color pic = do
  void $ forLoopState x1 (<= x2) (+1) (y1,di) $ \(y,d) x -> do
    writePoint color (Pair x y) $ pic
    return $ if d < 0 then (y,d+a+a) else (y+1,d+a+a+b+b)
  where a = y2 - y1
        b = x1 - x2
        di = a + a + b

writeLine' Second (Pair x1 y1) (Pair x2 y2) color pic = do
  void $ forLoopState y1 (<= y2) (+1) (x1,di) $ \(x,d) y -> do
    writePoint color (Pair x y) pic
    return $ if d > 0 then (x,d+b+b) else (x+1,d+a+a+b+b)
  where a = y2 - y1
        b = x1 - x2
        di = a + b + b

writeLine' Eighth (Pair x1 y1) (Pair x2 y2) color pic = do
  void $ forLoopState x1 (<= x2) (+1) (y1,di) $ \(y,d) x -> do
    writePoint color (Pair x y) pic
    return $ if d > 0 then (y,d+a+a) else (y-1,d+a+a-b-b)
  where a = y2 - y1
        b = x1 - x2
        di = a + a - b

writeLine' Seventh (Pair x1 y1) (Pair x2 y2) color pic = do
  void $ forLoopState y1 (>= y2) (\n -> n-1) (x1,di) $ \(x,d) y -> do
    writePoint color (Pair x y) pic
    return $ if d < 0 then (x,d-b-b) else (x+1,d+a+a-b-b)
  where a = y2 - y1
        b = x1 - x2
        di = a - b - b

drawColorLine :: Color -> Point -> Point -> Picture -> Picture
drawColorLine color p1 p2 = line p1 p2 color

drawLine :: Point -> Point -> Picture -> Picture
drawLine = drawColorLine white
