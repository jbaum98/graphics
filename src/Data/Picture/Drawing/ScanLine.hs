{-# LANGUAGE BangPatterns #-}

module Data.Picture.Drawing.ScanLine (
  scanLine, sortPoints
  ) where

import Debug.Trace

import Control.Monad

import Control.Monad.Primitive
import Control.Loop

import Data.Color
import Data.Pair
import Data.Picture.Drawing.Line (writeLine)
import Data.Picture.Drawing.Points (reflect)
import Data.Picture.Picture

scanLine :: PrimMonad m => Color -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Picture (PrimState m) -> m ()
scanLine color !x1 !y1 !z1 !x2 !y2 !z2 !x3 !y3 !z3 mpic = writeScanLine color xb yb zb xm ym zm xt yt zt mpic
  where
    Triple (Triple !xt !yt !zt) (Triple !xm !ym !zm) (Triple !xb !yb !zb) = sortPoints (Triple x1 y1 z1) (Triple x2 y2 z2) (Triple x3 y3 z3)
    --p1' = reflect' $ Triple x1 y1 z1
    --p2' = reflect' $ Triple x2 y2 z2
    --p3' = reflect' $ Triple x3 y3 z3
    --Pair _ yMax = getSize mpic
    --reflect' (Triple x y z) = case reflect yMax (Pair x y) of Pair x' y' -> Triple x' y' z
{-# INLINE scanLine #-}

{-               { left  :: {-# UNPACK #-} !(Pair Double)
               , right :: {-# UNPACK #-} !(Pair Double)
               , dL    :: {-# UNPACK #-} !(Pair Double)
               , dR    :: {-# UNPACK #-} !(Pair Double)
               , dR2   :: {-# UNPACK #-} !(Pair Double)
               , flipYet :: {-# UNPACK #-} !Bool }
-}
writeScanLine :: PrimMonad m => Color -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Picture (PrimState m) -> m ()
writeScanLine color xb yb zb xm ym zm xt yt zt mArr = void $
  --trace (show color) $ return ()
  --trace (show (round yb, round ym, round yt)) $ return ()
  numLoopState (truncate yb) (truncate yt) initState $ \(xl, zl, xr, zr, dxr, dzr, flipYet) y -> do
    writeLine (round xl) y zl (round xr) y zr color mArr
    --trace (show (xl,xr,y)) $ return ()
    if not flipYet && fromIntegral y + 1 >= ym
      then return (xl, zl, xm, zm, dxR2, dzR2, True)
      else return (xl + dxL, zl + dzL, xr + dxr, zr + dzr, dxr, dzr, flipYet)
  --trace "===============" $ return ()
  where
    initState = (xb, zb, xb, zb, dxR1, dzR1, False)
    dxL  = fSlope xb yb xt yt
    dzL  = fSlope zb yb zt yt
    dxR1 = fSlope xb yb xm ym
    dzR1 = fSlope zb yb zm ym
    dxR2 = fSlope xm ym xt yt
    dzR2 = fSlope zm ym zt yt

fSlope :: Double -> Double -> Double -> Double -> Double
fSlope x0 y0 x1 y1 = (x1 - x0) / (y1 - y0)

sortPoints :: Triple Double -> Triple Double -> Triple Double -> Triple (Triple Double)
sortPoints p1@(Triple _ !y1 _ ) p2@(Triple _ !y2 _) p3@(Triple _ !y3 _ ) | y1 >= y2 && y1 >= y3 && y2 >= y3 = Triple p1 p2 p3
sortPoints p1@(Triple _ !y1 _ ) p2@(Triple _ !y2 _) p3@(Triple _ !y3 _ ) | y1 >= y2 && y1 >= y3           = Triple p1 p3 p2

sortPoints p1@(Triple _ !y1 _ ) p2@(Triple _ !y2 _) p3@(Triple _ !y3 _ ) | y2 >= y1 && y2 >= y3 && y1 >= y3 = Triple p2 p1 p3
sortPoints p1@(Triple _ !y1 _ ) p2@(Triple _ !y2 _) p3@(Triple _ !y3 _ ) | y2 >= y1 && y2 >= y3           = Triple p2 p3 p1

sortPoints p1@(Triple _ !y1 _ ) p2@(Triple _ !y2 _) p3                   |                     y1 >= y2 = Triple p3 p1 p2
sortPoints p1              p2              p3                                                          = Triple p3 p2 p1
