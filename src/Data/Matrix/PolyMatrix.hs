{-# LANGUAGE ParallelListComp, BangPatterns #-}

module Data.Matrix.PolyMatrix (
  PolyMatrix(..),
  poly,
  addPoly
  ) where

import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as M

import Data.Matrix.Base
import Data.Matrix.ShapeMatrix
import Data.Pair
import Data.Color
import Data.Picture.Drawing.ScanLine
import Data.Picture.Drawing.Line
import Data.Picture.Drawing.Lighting (calcLighting, vertexNormals)
import Control.Monad

newtype PolyMatrix = PolyMatrix { runPM :: Matrix Double }

v :: Triple Double
v = Triple 0 0 1

instance ShapeMatrix PolyMatrix where
  drawColor color _ _ (PolyMatrix m) pic = forM_ [ (p1,p2,p3)
                               | i <- [0,3.. cols m - 2],
                                 let p1 = getD3Point m i
                                     p2 = getD3Point m $ i + 1
                                     p3 = getD3Point m $ i + 2
                                     n = p2 - p1 `cross` p3 - p1,
                                 n `dot` v > 0
                               ] $ \(p1,p2,p3) -> connect p1 p2 pic >> connect p2 p3 pic >> connect p3 p1 pic >> scan p1 p2 p3 pic
    where
      scan (Triple !x1 !y1 !z1) (Triple !x2 !y2 !z2) (Triple !x3 !y3 !z3) = scanLineFlat (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) color
      connect (Triple !x !y !z) (Triple !x' !y' !z') = drawColorLine color (round x) (round y) z (round x') (round y') z'

  draw lightinfo@(lighting,(Triple kr kg kb)) s (PolyMatrix m) pic =
    forM_ [ (p1,p2,p3,n)
          | i <- [0,3.. cols m - 2],
            let p1 = getD3Point m i
                p2 = getD3Point m $ i + 1
                p3 = getD3Point m $ i + 2
                n = p2 - p1 `cross` p3 - p1
          , n `dot` v > 0
    ] $ \(p1,p2,p3,n) -> scan n p1 p2 p3 pic
    where
      scan n (Triple !x1 !y1 !z1) (Triple !x2 !y2 !z2) (Triple !x3 !y3 !z3) = scanLine lightinfo s v n normals (x1,y1,z1) (x2,y2,z2) (x3,y3,z3)
      normals = vertexNormals $ PolyMatrix m
      connect3 color p1 p2 p3 p = connect color p1 p2 p >> connect color p2 p3 p >> connect color p3 p1 p
      connect color (Triple !x !y !z) (Triple !x' !y' !z') = drawColorLine color (round x) (round y) z (round x') (round y') z'

  unwrap = runPM
  wrap = PolyMatrix

instance Monoid PolyMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

poly :: Triple Double -> Triple Double -> Triple Double -> PolyMatrix
poly (Triple x1 y1 z1) (Triple x2 y2 z2) (Triple x3 y3 z3) =
  PolyMatrix $ Matrix 4 3 11 $
  V.fromList [x1, y1, z1, 1, x2, y2, z2, 1, x3, y3, z3, 1]

addPoly :: Triple Double -> Triple Double -> Triple Double -> PolyMatrix -> PolyMatrix
addPoly p1 p2 p3 = wrap .  addP p3 . addP p2 . addP p1 . unwrap
