module Matrix.EdgeMatrix (
    EdgeMatrix,
    edge,
    addEdge,
    ) where

import qualified Data.Vector.Unboxed as V

import Matrix.Base
import Matrix.D3Point
import Matrix.ShapeMatrix
import Picture
import Utils

newtype EdgeMatrix = EdgeMatrix { runEM :: Matrix D3Coord }

instance ShapeMatrix EdgeMatrix where
  drawColor color (EdgeMatrix m) = compose [drawColorLine color (getD2Point m i) (getD2Point m (i+1)) | i <- [0,2.. cols m - 1]]
  unwrap = runEM
  wrap = EdgeMatrix

instance Monoid EdgeMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

edge :: D3Point -> D3Point -> EdgeMatrix
edge (Triple x1 y1 z1) (Triple x2 y2 z2) = EdgeMatrix $ Matrix 4 2 7 $
  V.fromList [x1, y1, z1, 1, x2, y2, z2, 1]

addEdge :: D3Point -> D3Point -> EdgeMatrix -> EdgeMatrix
addEdge p1 p2 = wrap . addP p2 . addP p1 . unwrap
