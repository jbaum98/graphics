{-# LANGUAGE ParallelListComp, BangPatterns #-}

module Data.Picture.Drawing.ShapeMatrix.PolyMatrix (
  PolyMatrix(..),
  addPoly
  ) where

import Control.Monad
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

import Data.Color
import Data.Matrix
import Data.Pair
import Data.Picture.Drawing.Lighting (calcLighting, vertexNormals)
import Data.Picture.Drawing.Line
import Data.Picture.Drawing.ScanLine
import Data.Picture.Drawing.ShapeMatrix.ShapeMatrix

newtype PolyMatrix = PolyMatrix { runPM :: Matrix Double }

v :: Triple Double
v = Triple 0 0 1

instance ShapeMatrix PolyMatrix where
  drawColor color _ _ (PolyMatrix m) pic = forM_ [ (p1,p2,p3)
                               | i <- [0,3.. cols m - 2],
                                 let p1 = get3DPoint m i
                                     p2 = get3DPoint m $ i + 1
                                     p3 = get3DPoint m $ i + 2
                                     n = p2 - p1 `cross` p3 - p1,
                                 n `dot` v > 0
                               ] $ \(p1,p2,p3) -> connect p1 p2 pic >> connect p2 p3 pic >> connect p3 p1 pic >> scan p1 p2 p3 pic
    where
      scan (Triple !x1 !y1 !z1) (Triple !x2 !y2 !z2) (Triple !x3 !y3 !z3) = scanLineFlat (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) color
      connect (Triple !x !y !z) (Triple !x' !y' !z') = drawColorLine color (round x) (round y) z (round x') (round y') z'

  draw lightinfo@(lighting,(Triple kr kg kb)) s (PolyMatrix m) pic =
    forM_ [ (p1,p2,p3,n)
          | i <- [0,3.. cols m - 2],
            let p1 = get3DPoint m i
                p2 = get3DPoint m $ i + 1
                p3 = get3DPoint m $ i + 2
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

addPoly :: Triple Double -> Triple Double -> Triple Double -> PolyMatrix -> PolyMatrix
addPoly p1 p2 p3 = wrap .  addP p3 . addP p2 . addP p1 . unwrap
