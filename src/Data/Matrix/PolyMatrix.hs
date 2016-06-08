{-# LANGUAGE ParallelListComp, BangPatterns #-}

module Data.Matrix.PolyMatrix (
  PolyMatrix,
  poly,
  addPoly
  ) where

import Data.Monoid
import qualified Data.Vector.Unboxed as V

import Data.D3Point
import Data.Matrix.Base
import Data.Matrix.ShapeMatrix
import Data.Pair
import Data.Color
import Data.Picture.Drawing.ScanLine
import Data.Picture.Drawing.Line
import Control.Monad
import Data.Lighting

newtype PolyMatrix = PolyMatrix { runPM :: Matrix D3Coord }

instance ShapeMatrix PolyMatrix where
  drawColor color _ (PolyMatrix m) pic = forM_ [ (p1,p2,p3)
                               | i <- [0,3.. cols m - 2],
                                 let p1 = getD3Point m i
                                     p2 = getD3Point m $ i + 1
                                     p3 = getD3Point m $ i + 2
                                     n = p2 - p1 `cross` p3 - p1,
                                 n `dot` v < 0
                               ] $ \(p1,p2,p3) -> connect p1 p2 pic >> connect p2 p3 pic >> connect p3 p1 pic >> scan p1 p2 p3 pic
    where
      scan (Triple !x1 !y1 !z1) (Triple !x2 !y2 !z2) (Triple !x3 !y3 !z3) = scanLine color x1 y1 z1 x2 y2 z2 x3 y3 z3
      connect (Triple !x !y !z) (Triple !x' !y' !z') = drawColorLine color (round x) (round y) z (round x') (round y') z'
      v = Triple 0 0 (-1)

  draw (lighting,(Triple kr kg kb)) (PolyMatrix m) pic =
    forM_ [ (p1,p2,p3,color)
          | i <- [0,3.. cols m - 2],
            let p1 = getD3Point m i
                p2 = getD3Point m $ i + 1
                p3 = getD3Point m $ i + 2
                n = p2 - p1 `cross` p3 - p1
                n' = normalize n
                iamb = kamb * (fromIntegral <$> ambient lighting)
                idiff = kdiff * diffLighting (lights lighting) n'
                ispec = pure 0
                color = trunc <$> (iamb + idiff + ispec)
          , n `dot` v < 0
    ] $ \(p1,p2,p3,color) -> connect3 color p1 p2 p3 pic << scan color p1 p2 p3 pic
    where
      scan color (Triple !x1 !y1 !z1) (Triple !x2 !y2 !z2) (Triple !x3 !y3 !z3) = scanLine color x1 y1 z1 x2 y2 z2 x3 y3 z3
      connect3 color p1 p2 p3 p = connect color p1 p2 p >> connect color p2 p3 p >> connect color p3 p1 p
      connect color (Triple !x !y !z) (Triple !x' !y' !z') = drawColorLine color (round x) (round y) z (round x') (round y') z'
      v = Triple 0 0 (-1)
      kamb = Triple kar kag kab
      kdiff = Triple kdr kdg kdb
      kspec = Triple ksr ksg ksb
      Triple kar kdr ksr = kr
      Triple kag kdg ksg = kg
      Triple kab kdb ksb = kb

  unwrap = runPM
  wrap = PolyMatrix

diffLighting :: [PointLight] -> Triple Double -> Triple Double
diffLighting pls n = getSum $ flip foldMap pls $ \l ->
  let v' = normalize $ loc l
  in Sum $ pure (v' `dot` n) * (fromIntegral <$> color l)


(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

trunc :: Double -> ColorVal
trunc = truncate . boundColor

boundColor :: Double -> Double
boundColor n | n < 0 = 0
boundColor n | n > 255 = 255
boundColor n = n


instance Monoid PolyMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

poly :: D3Point -> D3Point -> D3Point -> PolyMatrix
poly (Triple x1 y1 z1) (Triple x2 y2 z2) (Triple x3 y3 z3) =
  PolyMatrix $ Matrix 4 3 11 $
  V.fromList [x1, y1, z1, 1, x2, y2, z2, 1, x3, y3, z3, 1]

addPoly :: D3Point -> D3Point -> D3Point -> PolyMatrix -> PolyMatrix
addPoly p1 p2 p3 = wrap .  addP p3 . addP p2 . addP p1 . unwrap
