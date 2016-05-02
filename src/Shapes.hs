{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Shapes (
  parametric,
  circle,
  hermite,
  bezier,
  box,
  torus,
  sphere
  ) where

import Matrix
import Data.Array.Repa hiding ((++))
import Prelude hiding ((++))

parametric :: (Double -> D3Point) -> EdgeMatrix
parametric f = fromFunction (ix2 4 (2 * steps)) $ \(Z :. r :. c) ->
  pointToMatF r $ f $ fromIntegral ((c + 1) `quot` 2) / fromIntegral steps
  where steps = 1000

circle :: D3Point -> D3Coord -> EdgeMatrix
circle (Triple cx cy cz) r = parametric f
  where f t = Triple x y cz
          where x = cx + r * cos (2 * pi * t)
                y = cy + r * sin (2 * pi * t)

hermite, bezier :: D3Point
                   -> D3Point
                   -> D3Point
                   -> D3Point
                   -> EdgeMatrix

hermite p0 r0 p1 r1 = matCurve hermMat p0 p1 r0 r1
  where hermMat = fromListUnboxed (ix2 4 4)
          [  2, -2,  1,  1
          , -3,  3, -2, -1
          ,  0,  0,  1,  0
          ,  1,  0,  0,  0 ]

bezier = matCurve bezMat
  where bezMat = fromListUnboxed (ix2 4 4)
          [ -1,  3, -3, 1
          ,  3, -6,  3, 0
          , -3,  3,  0, 0
          ,  1,  0,  0, 0 ]

box :: D3Point -> D3Point -> EdgeMatrix
box botLeft (Triple x y z) = fromPoints
  [ botLeft, topLeft
  , topLeft, topRight
  , topRight, botRight
  , botRight, botLeft
  , botLeft, botLeft + d
  , topLeft, topLeft + d
  , botRight, botRight + d
  , topRight, topRight + d
  , botLeft + d, topLeft + d
  , topLeft + d, topRight + d
  , topRight + d, botRight + d
  , botRight + d, botLeft + d
  ]
  where
    topLeft = botLeft + Triple 0 y 0
    botRight = botLeft + Triple x 0 0
    topRight = botLeft + Triple x y 0
    d = Triple 0 0 z

torus :: D3Point -> D3Coord -> D3Coord -> EdgeMatrix
torus c r1 r2 = foldl1 (++) circles
  where
    circles = take steps $ trans <$> iterate (rotBy $ 360.0 / fromIntegral steps) circ0
    trans = matMult $ transMatrix c
    circ0 = circle (Triple r2 0 0) r1
    steps = 100

sphere :: D3Point -> D3Coord -> EdgeMatrix
sphere c r  = foldl1 (++) circles
  where
    circles = take steps $ trans <$> iterate (rotBy $ 180 / fromIntegral steps) circ0
    circ0 = circle (Triple 0 0 0) r
    trans = matMult $ transMatrix c
    steps = 15

rotBy a = matMult $ rotYMatrix a

matCurve :: Matrix U D3Coord -> D3Point -> D3Point -> D3Point -> D3Point -> EdgeMatrix
matCurve cMat p1 p2 p3 p4 = parametric $ \t -> let t' = pure t in t' * (t' * (t' * a + b) + c) + d
  where
    [a,b,c,d] = matToPoints m
    !m = cMat `matMult` pointMat
    pointMat = fromListUnboxed (ix2 4 3) $ concatMap explode [p1, p2, p3, p4]
    explode (Triple x y z) = [x, y, z]

matToPoints :: EdgeMatrix -> [D3Point]
matToPoints em = fmap getPointN [0..n-1]
  where
    getPointN x = (\i -> em ! ix2 x i) <$> Triple 0 1 2
    (Z :. n :. _) = extent em

pointToMatF :: Int -> D3Point -> D3Coord
{-# INLINE pointToMatF #-}
pointToMatF 0 (Triple x _ _) = x
pointToMatF 1 (Triple _ y _) = y
pointToMatF 2 (Triple _ _ z) = z
pointToMatF 3 Triple {}      = 1
pointToMatF _ Triple {}      = 0
