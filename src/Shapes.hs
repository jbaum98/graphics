{-# LANGUAGE BangPatterns, FlexibleContexts #-}

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
  let pInitial = f 0
  (emFinal, _) <- forLoopState step (<= 1) (+ step) (em, pInitial) $ \(em',p) i -> do
    let p' = f i
    return (addEdge p p' em', p')
  return emFinal

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

addBox :: D3Point -> D3Point -> EdgeMatrix -> EdgeMatrix
addBox botLeft (Triple x y z) em = runST $ do
  (emFinal, _) <- numLoopState 0 11 (em, boxCmds) $ \(em', cmd:rest) _ -> do
    let em'' = cmd em'
    return (em'', rest)
  return emFinal
  where
    boxCmds =
      [ addEdge botLeft topLeft
      , addEdge topLeft topRight
      , addEdge topRight botRight
      , addEdge botRight botLeft
      , addEdge botLeft $ botLeft + d
      , addEdge topLeft $ topLeft + d
      , addEdge botRight $ botRight + d
      , addEdge topRight $ topRight + d
      , addEdge (botLeft + d) $ topLeft + d
      , addEdge (topLeft + d) $ topRight + d
      , addEdge (topRight + d) $ botRight + d
      , addEdge (botRight + d) $ botLeft + d
      ]
    topLeft = botLeft + Triple 0 y 0
    botRight = botLeft + Triple x 0 0
    topRight = botLeft + Triple x y 0
    d = Triple 0 0 z

addTorus :: D3Point -> D3Coord -> D3Coord -> Double -> EdgeMatrix -> EdgeMatrix
addTorus c r1 r2 step em = runST $ do
  let circ0 = addCircle (Triple r2 0 0) r1 step empty
  numLoopState 0 steps em $ \em' i ->
    return $ transMatrix c `matMult` rotYMatrix (360.0 / steps * i) `matMult` circ0  `mergeCols` em'
  where
    steps = 100

addSphere :: D3Point -> D3Coord -> Double -> EdgeMatrix -> EdgeMatrix
addSphere c r step em = runST $ do
  let circ0 = addCircle (Triple 0 0 0) r step empty
  numLoopState 0 steps em $ \em' i ->
    return $ transMatrix c `matMult` rotYMatrix (180.0 / steps * i) `matMult` circ0 `mergeCols` em'
  where
    steps = 30

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
