{-# LANGUAGE BangPatterns #-}

module Shapes (
  parametric,
  circle,
  hermite,
  bezier,
  box
  ) where

import Matrix
import Prelude hiding ((++))
import Data.Array.Repa hiding ((++))

parametric :: (Double -> D3Point) -> EdgeMatrix
parametric f = fromFunction (ix2 4 20000) $ \(Z :. r :. c) ->
  pointToMatF r $ f (0.0001 * fromIntegral ((c + 1) `quot` 2))

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
