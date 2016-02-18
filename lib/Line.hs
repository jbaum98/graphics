module Line (
  line,
  module Point
  ) where

import Point
import Control.Applicative ((<$>))

line :: Point -> Point -> [Point]
line p1@(Pair x1 y1) p2@(Pair x2 y2) = oct p1 p2
  where oct = if x2 > x1
              then if y2 > y1 -- Right Half
                   then if dx > dy -- Q1
                        then first
                        else second
                   else if dx > -dy -- Q4
                        then eighth
                        else seventh
              else if y2 > y1 -- Left Half
                   then if -dx > dy -- Q2
                        then fourth
                        else third
                   else if -dx > -dy -- Q3
                        then fifth
                        else sixth
        dx = x2 - x1
        dy = y2 - y1


getX :: Pair a -> a
getX (Pair x _) = x

getY :: Pair a -> a
getY (Pair _ y) = y

first :: Point -> Point -> [Point]
first p1@(Pair x1 y1) p2@(Pair x2 y2) = iter di [p1]
  where
    a = y2 - y1
    b = x1 - x2
    di = a + a + b

    iter oldD soFar@(lastP:_)
      | getX lastP < getX p2 = iter newD (nextP:soFar)
      | otherwise = soFar
      where (newD, nextP) =
              if oldD > 0
              then (oldD + a + a + b + b, Pair (x+1) (y+1))
              else (oldD + a + a,         Pair (x+1) y)
            Pair x y = lastP

second :: Point -> Point -> [Point]
second p1@(Pair x1 y1) p2@(Pair x2 y2) = iter di [p1]
  where
    a = y2 - y1
    b = x1 - x2
    di = a + b + b

    iter oldD soFar@(lastP:_)
      | getY lastP < getY p2 = iter newD (nextP:soFar)
      | otherwise = soFar
      where (newD, nextP) =
              if oldD < 0
              then (oldD + a + a + b + b, Pair (x+1) (y+1))
              else (oldD         + b + b, Pair  x    (y+1))
            (Pair x y) = lastP

third :: Point -> Point -> [Point]
third = flip seventh

fourth :: Point -> Point -> [Point]
fourth = flip eighth

fifth :: Point -> Point -> [Point]
fifth = flip first

sixth :: Point -> Point -> [Point]
sixth = flip second

seventh :: Point -> Point -> [Point]
seventh p1@(Pair x1 y1) p2@(Pair x2 y2) = iter di [p1]
  where
    a = y2 - y1
    b = x1 - x2
    di = a - b - b

    iter oldD soFar@(lastP:_)
      | getY lastP > getY p2 = iter newD (nextP:soFar)
      | otherwise = soFar
      where (newD, nextP) =
              if oldD > 0
              then (oldD + a + a - b - b, Pair (x+1) (y-1))
              else (oldD         - b - b, Pair  x    (y-1))
            (Pair x y) = lastP

eighth :: Point -> Point -> [Point]
eighth p1@(Pair x1 y1) p2@(Pair x2 y2) = iter di [p1]
  where
    a = y2 - y1
    b = x1 - x2
    di = a + a - b

    iter oldD soFar@(lastP:_)
      | getX lastP < getX p2 = iter newD (nextP:soFar)
      | otherwise = soFar
      where (newD, nextP) =
              if oldD < 0
              then (oldD + a + a - b - b, Pair (x+1) (y-1))
              else (oldD + a + a,         Pair (x+1) y)
            Pair x y = lastP
