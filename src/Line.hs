{-|
Module      : Line
Description : Draw lines between two 'Point's

Provides an implementation of Bresenham's Line Algorithm.
-}
module Line (
  Point, Pair(..),
  line
  ) where

import Point

data Octant = First | Second | Third   | Fourth |
              Fifth | Sixth  | Seventh | Eighth

-- |Draw a line between two 'Point's
line :: Point -- ^Starting point
     -> Point -- ^Ending point
     -> [Point] -- ^A list of 'Point's that make up the line
line p1@(Pair x1 y1) p2@(Pair x2 y2) = line' oct p1 p2
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

line' :: Octant -> Point -> Point -> [Point]
line' Third  p1 p2 = line' Seventh p2 p1
line' Fourth p1 p2 = line' Eighth  p2 p1
line' Fifth  p1 p2 = line' First   p2 p1
line' Sixth  p1 p2 = line' Second  p2 p1

line' oct p1@(Pair x1 y1) p2@(Pair x2 y2) = iter di [p1]
  where a = y2 - y1
        b = x1 - x2
        di = case oct of First   -> a + a + b
                         Second  -> a + a + b
                         Seventh -> a + a - b
                         Eighth  -> a + a - b
        iter = iterMaker oct p2 a b

iterMaker :: (Num a, Ord a) => Octant -> Point -> a -> a -> (a -> [Point] -> [Point])
iterMaker oct p2 a b = iter
  where iter oldD soFar@(lastP:_)
          | stillGoing lastP = iter newD (nextP:soFar)
          | otherwise = soFar
          where (newD, nextP) =
                  if oldD `ord` 0
                  then (oldD + dOffset + extraD, upper lastP)
                  else (oldD + dOffset,          lower lastP)

        stillGoing lastP = case oct of First   -> getX lastP < getX p2
                                       Second  -> getY lastP < getY p2
                                       Seventh -> getY lastP > getY p2
                                       Eighth  -> getX lastP < getX p2

        ord = case oct of First   -> (>)
                          Second  -> (<)
                          Seventh -> (>)
                          Eighth  -> (<)

        dOffset = case oct of First   -> a + a
                              Second  -> b + b
                              Seventh -> -b - b
                              Eighth  -> a + a

        extraD = case oct of First   -> b + b
                             Second  -> a + a
                             Seventh -> a + a
                             Eighth  -> -b - b

        upper (Pair x y) = case oct of First   -> Pair (x+1) (y+1)
                                       Second  -> Pair (x+1) (y+1)
                                       Seventh -> Pair (x+1) (y-1)
                                       Eighth  -> Pair (x+1) (y-1)

        lower (Pair x y) = case oct of First   -> Pair (x+1)  y
                                       Second  -> Pair  x    (y+1)
                                       Seventh -> Pair  x    (y-1)
                                       Eighth  -> Pair (x+1)  y