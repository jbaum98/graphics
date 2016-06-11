{-# LANGUAGE BangPatterns, ViewPatterns #-}

module Data.Picture.Drawing.ScanLine (
  scanLine, sortPoints
  ) where

import Debug.Trace

import Control.Monad

import Control.Monad.Primitive
import Control.Loop

import Data.Picture.Drawing.Lighting
import Data.Color
import Data.Pair
import Data.Picture.Drawing.Line (writeLine)
import Data.Picture.Picture

type Point = (Double,Double,Double,Triple Double)

scanLine :: PrimMonad m
         => (Lighting, LightingConsts)
         -> Triple Double
         -> Point -> Point -> Point
         -> Picture (PrimState m) -> m ()
scanLine l v p1 p2 p3 = writeScanLine l v pb pm pt
  where
    Triple pt pm pb = sortPoints p1 p2 p3
{-# INLINE scanLine #-}

writeScanLine :: PrimMonad m
              => (Lighting, LightingConsts)
              -> Triple Double
              -> Point -> Point -> Point
              -> Picture (PrimState m) -> m ()
writeScanLine l v (!xb,!yb,!zb,!nb) (!xm,!ym,!zm,!nm) (!xt,!yt,!zt,!nt) mArr = void $ do
  --trace (show (xb,yb,zb) ++ " => " ++ show cb) $ return ()
  --trace (show (xm,ym,zm) ++ " => " ++ show cm) $ return ()
  --trace (show (xt,yt,zt) ++ " => " ++ show ct) $ return ()
  --trace (show (dcL,dcR1,dcR2)) $ return ()
  forLoopState (round yb) (<= round yt) (+1) initState $ \((xl,zl,cl), (xr,zr,cr), (dxr,dzr,dcr), flipYet) y -> do
    let Pair (xl',zl',cl') (xr',zr',cr') = if xl < xr then Pair (xl,zl,cl) (xr,zr,cr) else Pair (xr,zr,cr) (xl,zl,cl)
    writeLine (floor xl') y zl' cl' (ceiling xr') y zr' cr' mArr
    if not flipYet && fromIntegral y + 1 >= ym
      then return ((xl,zl,cl), (xm,zm,cm), (dxR2,dzR2,dcR2), True)
      else return ((xl + dxL, zl + dzL, cl + dcL), (xr + dxr, zr + dzr, cr + dcr), (dxr, dzr, dcr), flipYet)
  where
    initState = ((xb,zb,cb),(xb,zb,cb),(dxR1,dzR1,dcR1), False)
    dxL  = fSlope xb yb xt yt
    dzL  = fSlope zb yb zt yt
    dcL  = vSlope cb yb ct yt
    dxR1 = fSlope xb yb xm ym
    dzR1 = fSlope zb yb zm ym
    dcR1 = vSlope cb yb cm ym
    dxR2 = fSlope xm ym xt yt
    dzR2 = fSlope zm ym zt yt
    dcR2 = vSlope cm ym ct yt
    cb = fromIntegral <$> calcLighting l nb v
    cm = fromIntegral <$> calcLighting l nm v
    ct = fromIntegral <$> calcLighting l nt v

{-# INLINE writeScanLine #-}

fSlope :: Double -> Double -> Double -> Double -> Double
fSlope x0 y0 x1 y1 = (x1 - x0) / (y1 - y0)
{-# INLINE fSlope #-}

vSlope :: Triple Double -> Double -> Triple Double -> Double -> Triple Double
vSlope x0 y0 x1 y1 = (x1 - x0) / pure (y1 - y0)
{-# INLINE vSlope #-}

sortPoints :: Point -> Point -> Point -> Triple Point
sortPoints p1@(getY -> !y1) p2@(getY -> !y2) p3@(getY -> !y3) | y1 >= y2 && y1 >= y3 && y2 >= y3 = Triple p1 p2 p3
sortPoints p1@(getY -> !y1) p2@(getY -> !y2) p3@(getY -> !y3) | y1 >= y2 && y1 >= y3           = Triple p1 p3 p2

sortPoints p1@(getY -> !y1) p2@(getY -> !y2) p3@(getY -> !y3) | y2 >= y1 && y2 >= y3 && y1 >= y3 = Triple p2 p1 p3
sortPoints p1@(getY -> !y1) p2@(getY -> !y2) p3@(getY -> !y3) | y2 >= y1 && y2 >= y3           = Triple p2 p3 p1

sortPoints p1@(getY -> !y1) p2@(getY -> !y2) p3              |                     y1 >= y2 = Triple p3 p1 p2
sortPoints p1              p2              p3                                            = Triple p3 p2 p1
{-# INLINE sortPoints #-}

getY :: Point -> Double
getY (_,y,_,_) = y
