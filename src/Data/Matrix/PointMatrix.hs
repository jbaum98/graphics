module Data.Matrix.PointMatrix (
  PointMatrix,
  point,
  addPoint
  ) where

import qualified Data.Vector.Unboxed as V

import Data.D3Point
import Data.Matrix.Base
import Data.Matrix.ShapeMatrix
import Data.Picture.Drawing.Points

newtype PointMatrix = PointMatrix { runPM :: Matrix D3Coord }

instance ShapeMatrix PointMatrix where
  drawColor color (PointMatrix m) = writePoints color [getD2Point m i | i <- [0..cols m - 1]]
  unwrap = runPM
  wrap = PointMatrix

instance Monoid PointMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

point :: D3Point -> PointMatrix
point (Triple x y z) = PointMatrix $ Matrix 4 1 3 $ V.fromList [x, y, z, 1]

addPoint :: D3Point -> PointMatrix -> PointMatrix
addPoint p = wrap . addP p . unwrap
