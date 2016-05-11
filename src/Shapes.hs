{-# LANGUAGE BangPatterns, FlexibleContexts, TupleSections #-}

module Shapes (
    addParametric,
    addCircle,
    addHermite,
    addBezier,
    addBox,
    addTorus,
    addSphere
  ) where

import Control.Monad.ST

import qualified Data.Vector.Unboxed as V
import Control.Loop

import Matrix
import Utils

addParametric :: Double -> (Double -> D3Point) -> EdgeMatrix -> EdgeMatrix
addParametric step f em = runST $ do
  (emFinal, _) <- forLoopState step (<= 1) (+ step) (em, f 0) $ \(em',p) i -> do
    let p' = f i
    return (addEdge p p' em', p')
  return emFinal
  -- compose [addEdge p p' | t <- [0,step..1], let (Pair p p') = f <$> Pair t (t + step)]

addParametric2 :: Int -> (Double -> Double -> D3Point) -> EdgeMatrix -> EdgeMatrix
addParametric2 steps f = compose [ addPoint $ f i j
                                 | i <- [0,step..1], j <- [0,step..1]]
  where step = recip $ fromIntegral steps - 1

addCircle :: D3Point -> D3Coord -> Double -> EdgeMatrix -> EdgeMatrix
addCircle (Triple cx cy cz) r step = addParametric step f .
                                     addEdge firstPoint lastPoint
  where
    f t = Triple (x t) (y t) cz
    x t = cx + r * cos (2 * pi * t)
    y t = cy + r * sin (2 * pi * t)
    firstPoint = Triple (cx + r) cy cz
    lastPoint = f $ 1 - step

addHermite, addBezier :: D3Point
                      -> D3Point
                      -> D3Point
                      -> D3Point
                      -> Double
                      -> EdgeMatrix
                      -> EdgeMatrix

addHermite p0 r0 p1 r1 = addMatCurve hermMat p0 p1 r0 r1
  where hermMat = fromLists [
          [  2, -2,  1,  1 ],
          [ -3,  3, -2, -1 ],
          [  0,  0,  1,  0 ],
          [  1,  0,  0,  0 ]
          ]

addBezier = addMatCurve bezMat
  where bezMat = fromLists [
          [ -1,  3, -3, 1 ],
          [  3, -6,  3, 0 ],
          [ -3,  3,  0, 0 ],
          [  1,  0,  0, 0 ]
          ]

addBox :: D3Point -> D3Point -> EdgeMatrix -> EdgeMatrix
addBox topLeft (Triple x y z) = compose [
       -- Front
       addPoly topLeft botLeft topRight
     , addPoly botRight topRight botLeft
       -- Back
     , addPoly (topRight + d) (botRight + d) (topLeft + d)
     , addPoly (botLeft + d) (topLeft + d) (botRight + d)
       -- Left
     , addPoly (topLeft + d) (botLeft + d) topLeft
     , addPoly botLeft topLeft (botLeft + d)
       -- Right
     , addPoly topRight botRight (topRight + d)
     , addPoly (botRight + d) (topRight + d) botRight
       -- Top
     , addPoly (topLeft + d) topLeft (topRight + d)
     , addPoly topRight (topRight + d) topLeft
       -- Bottom
     , addPoly botLeft (botLeft + d) botRight
     , addPoly (botRight + d) botRight (botLeft + d)
     ]
  where
    botLeft = topLeft - Triple 0 y 0
    botRight = botLeft + Triple x 0 0
    topRight = topLeft + Triple x 0 0
    d = Triple 0 0 (-z)

addTorus :: D3Point -> D3Coord -> D3Coord -> Int -> EdgeMatrix -> EdgeMatrix
addTorus c r1 r2 steps = crissCross steps connectSlice
  where
    torus = genTorus c r1 r2 steps
    connectSlice = connectShapeSlice (torus, steps)

genTorus :: D3Point -> D3Coord -> D3Coord -> Int -> Matrix D3Coord
genTorus c r1 r2 steps = addParametric2 steps f $ emptyWith (4 * steps * steps)
  where
    f t p = Triple (x t p) (y t p) (z t p) + c
    x thetaT phiT = cos(phiT * 2 * pi) * (r1 * cos(thetaT * 2 * pi) + r2)
    y thetaT _ = r1 * sin (thetaT * 2 * pi)
    z thetaT phiT = sin(phiT * 2 * pi) * (r1 * cos(thetaT * 2 * pi) + r2)

addSphere :: D3Point -> D3Coord -> Int -> EdgeMatrix -> EdgeMatrix
addSphere c r steps = connectMiddles . connectCaps
  where
    connectMiddles = crissCross steps connectSlice
    connectCaps = compose [connect (0, 1, steps + 1) . connect (steps - 2, steps - 1, steps + steps - 2) | sliceN <- [0..steps - 2], let connect = connectSlice sliceN]
    sphere = genSphere c r steps
    connectSlice = connectShapeSlice (sphere, steps)

crissCross :: Int -> (Int -> (Int, Int, Int) -> EdgeMatrix -> EdgeMatrix) -> EdgeMatrix -> EdgeMatrix
crissCross steps connectSlice = compose [connect (i,i+1, steps+i) . connect (i+1, steps+i+1, steps+i) | i <- [0..steps-2], sliceN <- [0..steps-2], let connect = connectSlice sliceN]

connectShapeSlice :: (Matrix D3Coord, Int) -> Int -> (Int, Int, Int) -> EdgeMatrix -> EdgeMatrix
connectShapeSlice (points,steps) sliceN (i1, i2, i3) = addPoly (point i1) (point i2) (point i3)
    where
      point n = V.unsafeIndex (slice sliceN) . (+ (n * 4)) <$> Triple 0 1 2
      slice = circSlice points
      circSlice m i = V.slice (steps * i * 4) (4*steps) (vector m)

genSphere :: D3Point -> D3Coord -> Int -> Matrix D3Coord
genSphere c r steps = addParametric2 steps f $ emptyWith (4 * steps * steps)
  where
    f t p = Triple (x t p) (y t p) (z t p) + c
    x thetaT _ = r * cos (thetaT * pi)
    y thetaT phiT = r*sin(thetaT * pi) * cos(phiT * 2 * pi)
    z thetaT phiT = r*sin(thetaT * pi) * sin(phiT * 2 * pi)

addMatCurve :: Matrix D3Coord -> D3Point -> D3Point -> D3Point -> D3Point -> Double -> EdgeMatrix -> EdgeMatrix
addMatCurve cMat p1 p2 p3 p4 step em = runST $ do
  let a = Triple (m ! (1,1)) (m ! (1,2)) (m ! (1,3))
      b = Triple (m ! (2,1)) (m ! (2,2)) (m ! (2,3))
      c = Triple (m ! (3,1)) (m ! (3,2)) (m ! (3,3))
      d = Triple (m ! (4,1)) (m ! (4,2)) (m ! (4,3))
      !m = cMat `matMult` pointMat
  return $ flip (addParametric step) em $ \t ->
    let t' = pure t in
    t' * (t' * (t' * a + b) + c) + d
  where
    pointMat = fromLists [
      explode p1,
      explode p2,
      explode p3,
      explode p4
      ]
    explode (Triple x y z) = [x, y, z]
