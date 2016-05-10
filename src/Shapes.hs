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

import Matrix.Base
import Matrix.EdgeMatrix
import Matrix.Transform
import Matrix.Mult
import Utils
import Debug.Trace (trace)
import Control.Loop
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive

addParametric :: Double -> (Double -> D3Point) -> EdgeMatrix -> EdgeMatrix
addParametric step f em = runST $ do
  (emFinal, _) <- forLoopState step (<= 1) (+ step) (em, f 0) $ \(em',p) i -> do
    let p' = f i
    return (addEdge p p' em', p')
  return emFinal

addParametric2 :: Int -> (Double -> Double -> D3Point) -> EdgeMatrix -> EdgeMatrix
addParametric2 steps f em = runST $
  let normalize = (/ fromIntegral (steps-1)) . fromIntegral in
  numLoopState 0 (steps-1) em $ \em' j ->
    numLoopState 0 (steps-1) em' $ \em'' i -> do
      let p = f (normalize i) (normalize j)
      return $ addPoint p em''

addCircle :: D3Point -> D3Coord -> Double -> EdgeMatrix -> EdgeMatrix
addCircle (Triple cx cy cz) r step = addParametric step f . addEdge (Triple (cx + r) cy cz) (f $ 1 - step)
  where
    f t = Triple (x t) (y t) cz
    x t = cx + r * cos (2 * pi * t)
    y t = cy + r * sin (2 * pi * t)

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

addPoints :: [D3Point] -> EdgeMatrix -> EdgeMatrix
addPoints = compose . fmap (\p -> addEdge p p)

addBox :: D3Point -> D3Point -> EdgeMatrix -> EdgeMatrix
addBox botLeft (Triple x y z) = addPoints points
  where
    points = [
      botLeft,
      topLeft,
      topRight,
      botRight,
      botLeft + d,
      topLeft + d,
      botRight + d,
      topRight + d
      ]
    topLeft = botLeft + Triple 0 y 0
    botRight = botLeft + Triple x 0 0
    topRight = botLeft + Triple x y 0
    d = Triple 0 0 z

addTorus :: D3Point -> D3Coord -> D3Coord -> Int -> EdgeMatrix -> EdgeMatrix
addTorus c r1 r2 steps = addParametric2 steps $ \t p -> Triple (x t p) (y t p) (z t p) + c
  where
    x thetaT _ = r1 * cos (thetaT * 2 * pi)
    y thetaT phiT = cos(phiT * 2 * pi) * (r1 * sin(thetaT * 2 * pi) + r2)
    z thetaT phiT = sin(phiT * 2 * pi) * (r1 * sin(thetaT * 2 * pi) + r2)

addSphere :: D3Point -> D3Coord -> Int -> EdgeMatrix -> EdgeMatrix
addSphere c r steps em = runST $ do
  (_,_,em') <- forLoopState 0 (< steps) (+1) (0,steps,em) $ \(i,i',em') _ -> do
    let addAll = addPoly (point i) (point $ i + 1) (point i') . addPoly (point $  i' - 2) (point $ i' - 1) (point $ i' + steps - 1) . compose [ addPoly (point $ i+j) (point $ i+j+1) (point $ i'+j+1) . addPoly (point $ i'+j+1) (point $ i'+j) (point $ i+j) | j <- [1..steps]]
    return (i', i' + steps, addAll em')
  return em'
  where
    points = genSphere c r steps
    point i = unsafeIndex points . (,i) <$> Triple 1 2 3

genSphere :: D3Point -> D3Coord -> Int -> EdgeMatrix
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

pointToMatF :: Int -> D3Point -> D3Coord
{-# INLINE pointToMatF #-}
pointToMatF 0 (Triple x _ _) = x
pointToMatF 1 (Triple _ y _) = y
pointToMatF 2 (Triple _ _ z) = z
pointToMatF 3 Triple {}      = 1
pointToMatF _ Triple {}      = 0
