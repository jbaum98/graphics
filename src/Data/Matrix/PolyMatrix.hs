{-# LANGUAGE ParallelListComp, BangPatterns #-}

module Data.Matrix.PolyMatrix (
  PolyMatrix,
  poly,
  addPoly
  ) where

import Debug.Trace

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

  draw lightinfo@(lighting,(Triple kr kg kb)) (PolyMatrix m) pic =
    forM_ [ (p1,p2,p3,color)
          | i <- [0,3.. cols m - 2],
            let p1 = getD3Point m i
                p2 = getD3Point m $ i + 1
                p3 = getD3Point m $ i + 2
                n = p2 - p1 `cross` p3 - p1
                color = calcLighting lightinfo n v
          , n `dot` v < 0
    ] $ \(p1,p2,p3,color) -> connect3 color p1 p2 p3 pic << scan color p1 p2 p3 pic
    where
      scan color (Triple !x1 !y1 !z1) (Triple !x2 !y2 !z2) (Triple !x3 !y3 !z3) = scanLine color x1 y1 z1 x2 y2 z2 x3 y3 z3
      connect3 color p1 p2 p3 p = connect color p1 p2 p >> connect color p2 p3 p >> connect color p3 p1 p
      connect color (Triple !x !y !z) (Triple !x' !y' !z') = drawColorLine color (round x) (round y) z (round x') (round y') z'
      v = Triple 0 0 (-1)

  unwrap = runPM
  wrap = PolyMatrix

calcLighting :: (Lighting,LightingConsts) -> Triple Double -> Triple Double -> Color
calcLighting (Lighting amb pointLights, Triple (Triple kar kdr ksr) (Triple kag kdg ksg) (Triple kab kdb ksb)) n v =
  (trunc <$> iamb) + (trunc <$> idiff) + (trunc <$> ispec)
  where
    n' = normalize n
    v' = normalize v
    iamb = kamb * (fromIntegral <$> amb)
    idiff = kdiff * diffLighting pointLights n'
    ispec = kspec * specLighting pointLights n' v'
    kamb = Triple kar kag kab
    kdiff = Triple kdr kdg kdb
    kspec = Triple ksr ksg ksb

diffLighting :: [PointLight] -> Triple Double -> Triple Double
diffLighting pls n = getSum $ flip foldMap pls $ \l ->
  let l' = negate $ normalize $ loc l
  in Sum $ pure (l' `dot` n) * (fromIntegral <$> color l)

specLighting :: [PointLight] -> Triple Double -> Triple Double -> Triple Double
specLighting pls n v = getSum $ flip foldMap pls $ \(PointLight c l) ->
  let l' = normalize l
      col = fromIntegral <$> c
      r = 2 * (n * pure (l' `dot` n)) - l'
  in Sum $
    pure ((r `dot` v) ^ k) * trace (show r) col
  where k = 8 :: Int

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

trunc :: (Integral b, Num a, Ord a, RealFrac a) => a -> b
trunc = truncate . boundColor

boundColor :: (Num a, Ord a) => a -> a
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
