module Line (
  first,
  module Point
  ) where

import Point

type LineState = (Point, Integer)

first :: Point -> Point -> [Point]
first p1 p2 = takeWhile (\p -> getX p <= getX p2) $ map getPoint $ first' p1 p2
  where getX (Pair x _) = x
        getPoint (p, _) = p

first' :: Point -> Point -> [LineState]
first' p1@(Pair x1 y1) p2@(Pair x2 y2) = (p1, di) : map nextState (first' p1 p2)
  where a = y2 - y1
        b = x1 - x2
        di = a + a + b
        nextState :: LineState -> LineState
        nextState lastState = (nextPoint, newD)
          where (Pair x y, d) = lastState
                nextPoint = if d > 0 then upper else lower
                upper = Pair (x+1) (y+1)
                lower = Pair (x+1) y
                newD =  if d > 0 then d + a + a + b + b else d + a + a
