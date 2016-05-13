module Matrix.PointMatrix (
  PointMatrix,
  point,
  addPoint
  ) where

import qualified Data.Vector.Unboxed as V

import Matrix.Base
import Matrix.D3Point
import Matrix.ShapeMatrix
import Picture
import Utils

newtype PointMatrix = PointMatrix { runPM :: Matrix D3Coord }

instance ShapeMatrix PointMatrix where
  drawColor color (PointMatrix m) = compose $ fmap (setPointColor color. getD2Point m) [0..cols m - 1]
  unwrap = runPM
  wrap = PointMatrix

point :: D3Point -> PointMatrix
point (Triple x y z) = PointMatrix $ Matrix 4 1 3 $ V.fromList [x, y, z, 1]

addPoint :: D3Point -> PointMatrix -> PointMatrix
addPoint p = wrap . addP p . unwrap
