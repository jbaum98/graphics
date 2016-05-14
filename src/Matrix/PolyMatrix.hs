module Matrix.PolyMatrix (
  PolyMatrix,
  poly,
  addPoly
  ) where

import qualified Data.Vector.Unboxed as V

import Matrix.Base
import Matrix.D3Point
import Matrix.ShapeMatrix
import Utils
import Pair
import Picture

newtype PolyMatrix = PolyMatrix { runPM :: Matrix D3Coord }

instance ShapeMatrix PolyMatrix where
  drawColor color (PolyMatrix m) = compose [connect p1 p2 . connect p2 p3 . connect p3 p1
                               | i <- [0,3.. cols m - 2],
                                 let p1 = getD3Point m i
                                     p2 = getD3Point m $ i + 1
                                     p3 = getD3Point m $ i + 2
                                     n = p2 - p1 `cross` p3 - p1,
                                 n `dot` v < 0
                               ]
    where
      connect (Triple x y _) (Triple x' y' _) = drawColorLine color (round <$> Pair x y) (round <$> Pair x' y')
      v = Triple 0 0 (-1)

  unwrap = runPM
  wrap = PolyMatrix

instance Monoid PolyMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

poly :: D3Point -> D3Point -> D3Point -> PolyMatrix
poly (Triple x1 y1 z1) (Triple x2 y2 z2) (Triple x3 y3 z3) =
  PolyMatrix $ Matrix 4 3 11 $
  V.fromList [x1, y1, z1, 1, x2, y2, z2, 1, x3, y3, z3, 1]

addPoly :: D3Point -> D3Point -> D3Point -> PolyMatrix -> PolyMatrix
addPoly p1 p2 p3 = wrap .  addP p3 . addP p2 . addP p1 . unwrap
