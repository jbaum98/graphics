{-# LANGUAGE BangPatterns, FlexibleContexts, TupleSections #-}

module Shapes (
    parametric,
    circle,
    hermite,
    bezier,
    box,
    torus,
    sphere
  ) where

import Control.Monad.ST

import qualified Data.Vector.Unboxed as V
import Control.Loop

import Matrix
import Utils

parametric :: Double -> (Double -> D3Point) -> EdgeMatrix
parametric step f = runST $ do
  (emFinal, _) <- forLoopState step (<= 1) (+ step) (em, f 0) $ \(em',p) i -> do
    let p' = f i
    return (addEdge p p' em', p')
  return emFinal
  where em = wrap $ emptyWith (round $ 4 / step)
  -- compose [addEdge p p' | t <- [0,step..1], let (Pair p p') = f <$> Pair t (t + step)]

parametric2 :: Int -> (Double -> Double -> D3Point) -> PointMatrix
parametric2 steps f = addPoints $ wrap $ emptyWith (4 * steps * steps)
  where
    addPoints = compose [ addPoint $ f i j
                        | j <- [0,step..1], i <- [0,step..1]]
    step = recip $ fromIntegral steps - 1

circle :: D3Point -> D3Coord -> Double -> EdgeMatrix
circle (Triple cx cy cz) r step = addEdge firstPoint lastPoint $ parametric step f
  where
    f t = Triple (x t) (y t) cz
    x t = cx + r * cos (2 * pi * t)
    y t = cy + r * sin (2 * pi * t)
    firstPoint = Triple (cx + r) cy cz
    lastPoint = f $ 1 - step

hermite, bezier :: D3Point
                  -> D3Point
                  -> D3Point
                  -> D3Point
                  -> Double
                  -> EdgeMatrix

hermite p0 r0 p1 r1 = matCurve hermMat p0 p1 r0 r1
  where hermMat = fromLists [
          [  2, -2,  1,  1 ],
          [ -3,  3, -2, -1 ],
          [  0,  0,  1,  0 ],
          [  1,  0,  0,  0 ]
          ]

bezier = matCurve bezMat
  where bezMat = fromLists [
          [ -1,  3, -3, 1 ],
          [  3, -6,  3, 0 ],
          [ -3,  3,  0, 0 ],
          [  1,  0,  0, 0 ]
          ]

matCurve :: Matrix D3Coord -> D3Point -> D3Point -> D3Point -> D3Point -> Double -> EdgeMatrix
matCurve cMat p1 p2 p3 p4 step = parametric step $ \t ->
  let t' = pure t
  in t' * (t' * (t' * a + b) + c) + d
  where
    a = Triple (m ! (1,1)) (m ! (1,2)) (m ! (1,3))
    b = Triple (m ! (2,1)) (m ! (2,2)) (m ! (2,3))
    c = Triple (m ! (3,1)) (m ! (3,2)) (m ! (3,3))
    d = Triple (m ! (4,1)) (m ! (4,2)) (m ! (4,3))
    !m = cMat `matMult` pointMat
    pointMat = fromLists [
      explode p1,
      explode p2,
      explode p3,
      explode p4
      ]
    explode (Triple x y z) = [x, y, z]

box :: D3Point -> D3Point -> PolyMatrix
box topLeft (Triple x y z) = addPolys $ wrap $ emptyWith 47
  where
    addPolys = compose [
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
    botLeft = topLeft - Triple 0 y 0
    botRight = botLeft + Triple x 0 0
    topRight = topLeft + Triple x 0 0
    d = Triple 0 0 (-z)

torus :: D3Point -> D3Coord -> D3Coord -> Int -> PolyMatrix
torus c r1 r2 steps = crissCross steps connectSlice $ wrap $ emptyWith (6 * steps * steps)
  where
    torusPoints = genTorus c r1 r2 steps
    connectSlice = connectShapeSlice (torusPoints, steps)

genTorus :: D3Point -> D3Coord -> D3Coord -> Int -> PointMatrix
genTorus c r1 r2 steps = parametric2 steps f
  where
    f t p = Triple (x t p) (y t p) (z t p) + c
    x thetaT phiT = cos(phiT * 2 * pi) * (r1 * cos(thetaT * 2 * pi) + r2)
    y thetaT _ = r1 * sin (thetaT * 2 * pi)
    z thetaT phiT = sin(phiT * 2 * pi) * (r1 * cos(thetaT * 2 * pi) + r2)

sphere :: D3Point -> D3Coord -> Int -> PolyMatrix
sphere c r steps = connectMiddles . connectCaps $ wrap $ emptyWith (6 * (steps - 2) * steps)
  where
    connectMiddles = crissCross steps connectSlice
    connectCaps = compose [connect (0, steps+1, 1) . connect (steps + steps - 2, steps - 1, steps - 2) | sliceN <- [0..steps - 2], let connect = connectSlice sliceN]
    spherePoints = genSphere c r steps
    connectSlice = connectShapeSlice (spherePoints, steps)

genSphere :: D3Point -> D3Coord -> Int -> PointMatrix
genSphere c r steps = parametric2 steps f
  where
    f t p = Triple (x t p) (y t p) (z t p) + c
    x thetaT _ = r * cos (thetaT * pi)
    y thetaT phiT = r*sin(thetaT * pi) * cos(phiT * 2 * pi)
    z thetaT phiT = r*sin(thetaT * pi) * sin(phiT * 2 * pi)

crissCross :: Int -> (Int -> (Int, Int, Int) -> PolyMatrix -> PolyMatrix) -> PolyMatrix -> PolyMatrix
crissCross steps connectSlice = compose [
  connect (i, i', i+1) .
  connect (i'+1, i+1, i')
  | i      <- [0..steps-2],
    sliceN <- [0..steps-2],
    let connect = connectSlice sliceN
        i'      = i + steps
  ]

connectShapeSlice :: (PointMatrix, Int) -> Int -> (Int, Int, Int) -> PolyMatrix -> PolyMatrix
connectShapeSlice (points,steps) sliceN (i1, i2, i3) = addPoly (getPoint i1) (getPoint i2) (getPoint i3)
    where
      getPoint n = V.unsafeIndex (slice sliceN) . (+ (n * 4)) <$> Triple 0 1 2
      slice = circSlice $ unwrap points
      circSlice m i = V.slice (steps * i * 4) (4*steps) (vector m)
