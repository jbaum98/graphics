{-# LANGUAGE BangPatterns #-}

module Data.Picture.Drawing.ShapeMatrix.PointMatrix (
  PointMatrix,
  addPoint
  ) where

import Data.Pair
import Data.Matrix
import Data.Picture.Drawing.ShapeMatrix.ShapeMatrix
import Data.Picture.Drawing.Points

newtype PointMatrix = PointMatrix { runPM :: Matrix Double }

instance ShapeMatrix PointMatrix where
  drawColor color _ _ (PointMatrix m) = writePoints color [case get3DPoint m i of Triple !x !y !z -> (Pair (round x) (round y), z)| i <- [0..cols m - 1]]
  unwrap = runPM
  wrap = PointMatrix

instance Monoid PointMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

addPoint :: Triple Double -> PointMatrix -> PointMatrix
addPoint p = wrap . addP p . unwrap
