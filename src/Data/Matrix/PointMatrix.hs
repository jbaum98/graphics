{-# LANGUAGE BangPatterns #-}

module Data.Matrix.PointMatrix (
  PointMatrix,
  point,
  addPoint
  ) where

import qualified Data.Vector.Unboxed as V

import Data.Pair
import Data.Matrix.Base
import Data.Matrix.ShapeMatrix
import Data.Picture.Drawing.Points

newtype PointMatrix = PointMatrix { runPM :: Matrix Double }

instance ShapeMatrix PointMatrix where
  drawColor color _ _ (PointMatrix m) = writePoints color [case getD3Point m i of Triple !x !y !z -> (Pair (round x) (round y), z)| i <- [0..cols m - 1]]
  unwrap = runPM
  wrap = PointMatrix

instance Monoid PointMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

point :: Triple Double -> PointMatrix
point (Triple x y z) = PointMatrix $ Matrix 4 1 3 $ V.fromList [x, y, z, 1]

addPoint :: Triple Double -> PointMatrix -> PointMatrix
addPoint p = wrap . addP p . unwrap
