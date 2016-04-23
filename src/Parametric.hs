{-# LANGUAGE BangPatterns #-}

module Parametric (
  addParametric,
  addCircle,
  addHermite,
  addBezier
  ) where

import Matrix
import Prelude hiding ((++))
import Data.Array.Repa hiding ((++))

addParametric :: (Double -> D3Point) -> EdgeMatrix -> EdgeMatrix
addParametric f = (++ edges)
  where
    edges = fromFunction (ix2 4 20000) h
    h (Z :. r :. c) = pointToMatF r $ f (0.0001 * fromIntegral ((c + 1) `quot` 2))

addCircle :: D3Point -> D3Coord -> EdgeMatrix -> EdgeMatrix
addCircle (Triple cx cy cz) r = addParametric f
  where f t = Triple x y cz
          where x = cx + r * cos (2 * pi * t)
                y = cy + r * sin (2 * pi * t)

addHermite, addBezier :: D3Point
                      -> D3Point
                      -> D3Point
                      -> D3Point
                      -> EdgeMatrix -> EdgeMatrix

addHermite p0 r0 p1 r1 = addMatCurve hermMat p0 p1 r0 r1
  where hermMat = fromListUnboxed (ix2 4 4)
          [  2, -2,  1,  1
          , -3,  3, -2, -1
          ,  0,  0,  1,  0
          ,  1,  0,  0,  0 ]

addBezier = addMatCurve bezMat
  where bezMat = fromListUnboxed (ix2 4 4)
          [ -1,  3, -3, 1
          ,  3, -6,  3, 0
          , -3,  3,  0, 0
          ,  1,  0,  0, 0 ]

addMatCurve :: Matrix U D3Coord -> D3Point -> D3Point -> D3Point -> D3Point -> EdgeMatrix -> EdgeMatrix
addMatCurve cMat p1 p2 p3 p4 = addParametric f
  where
    f t = t' * (t' * (t' * a + b) + c) + d
      where t' = pure t
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
