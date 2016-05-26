{-# LANGUAGE ViewPatterns, ParallelListComp, BangPatterns #-}

module Data.Picture.Drawing.ScanLine (
  scanLine, sortPoints
  ) where

import Control.Monad

import Control.Monad.Primitive
import Control.Loop

import Data.Color
import Data.Pair
import Data.Picture.Drawing.Line (writeLine)
import Data.Picture.Drawing.Points (reflect)
import Data.Picture.Picture

scanLine :: PrimMonad m => Color -> Double -> Double -> Double -> Double -> Double -> Double -> Picture (PrimState m) -> m ()
scanLine color !x1 !y1 !x2 !y2 !x3 !y3 mpic = writeScanLine color xb yb xm ym xt yt mpic
  where
    Triple (Pair !xt !yt) (Pair !xm !ym) (Pair !xb !yb) = sortPoints p1' p2' p3'
    p1' = reflect yMax $ Pair x1 y1
    p2' = reflect yMax $ Pair x2 y2
    p3' = reflect yMax $ Pair x3 y3
    Pair _ yMax = getSize mpic
{-# INLINE scanLine #-}

writeScanLine :: PrimMonad m => Color -> Double -> Double -> Double -> Double -> Double -> Double -> Picture (PrimState m) -> m ()
writeScanLine color xb yb xm ym xt yt mArr = do
  Pair l _ <- forLoopState yb (< ym) (+1) (pure xb) $ \(Pair xl xr) y -> do
    writeLine (round xl) (round y) (round xr) (round y) color mArr
    return $ Pair (xl + dxL) (xr + dxR1)
  void $ forLoopState ym (< yt) (+1) (Pair l xm) $ \(Pair xl xr) y -> do
    writeLine (round xl) (round y) (round xr) (round y) color mArr
    return $ Pair (xl+dxL) (xr+dxR2)
  where
    dxL  = fSlope xb yb xt yt
    dxR1 = fSlope xb yb xm ym
    dxR2 = fSlope xm ym xt yt

fSlope :: Double -> Double -> Double -> Double -> Double
fSlope x0 y0 x1 y1 = (x1 - x0) / (y1 - y0)

sortPoints :: Pair Double -> Pair Double -> Pair Double -> Triple (Pair Double)
sortPoints p1@(Pair _ !y1) p2@(Pair _ !y2) p3@(Pair _ !y3) | y1 >= y2 && y1 >= y3 && y2 >= y3 = Triple p1 p2 p3
sortPoints p1@(Pair _ !y1) p2@(Pair _ !y2) p3@(Pair _ !y3) | y1 >= y2 && y1 >= y3           = Triple p1 p3 p2

sortPoints p1@(Pair _ !y1) p2@(Pair _ !y2) p3@(Pair _ !y3) | y2 >= y1 && y2 >= y3 && y1 >= y3 = Triple p2 p1 p3
sortPoints p1@(Pair _ !y1) p2@(Pair _ !y2) p3@(Pair _ !y3) | y2 >= y1 && y2 >= y3           = Triple p2 p3 p1

sortPoints p1@(Pair _ !y1) p2@(Pair _ !y2) p3              |                     y1 >= y2 = Triple p3 p1 p2
sortPoints p1              p2              p3              | otherwise                   = Triple p3 p2 p1
