{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections #-}

module Data.Picture.Drawing.ScanLine (
  scanLine, sortPoints, scanLineFlat
  ) where

import Control.Monad

import Control.Monad.Primitive
import Control.Loop
import qualified Data.Map as M

import Data.Picture.Drawing.Lighting
import Data.Color
import Data.Pair
import Data.Picture.Drawing.Line (writeLine)
import Data.Picture.Picture

newtype Point           = P   (Double,Double,Double) deriving (Eq)

instance Ord Point where
  compare (P (_,y1,_)) (P (_,y2,_)) = compare y1 y2

newtype PointWithColor = PWC (Double,Double,Double,Triple Double) deriving (Eq)

instance Ord PointWithColor where
  compare (PWC (_,y1,_,_)) (PWC (_,y2,_,_)) = compare y1 y2

newtype PointWithNormal = PWN (Double,Double,Double,Triple Double) deriving (Eq)

instance Ord PointWithNormal where
  compare (PWN (_,y1,_,_)) (PWN (_,y2,_,_)) = compare y1 y2

scanLineFlat :: PrimMonad m => (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
             -> Color -> Picture (PrimState m) -> m ()
scanLineFlat p1 p2 p3 = flat pb pm pt . fmap fromIntegral
  where
    Triple pt pm pb = sortPoints (P p1) (P p2) (P p3)

scanLine :: PrimMonad m
         => (Lighting, LightingConsts) -> ShadingType
         -> Triple Double -> Triple Double
         -> M.Map (Triple Double) (Triple Double)
         -> (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
         -> Picture (PrimState m) -> m ()

scanLine l Flat v n _ p1 p2 p3 = flat pb pm pt $ fromIntegral <$> calcLighting l n v
  where
    Triple pt pm pb = sortPoints (P p1) (P p2) (P p3)


scanLine l Goroud v _ normals p1 p2 p3 = goroud pb pm pt
  where
    Triple pt pm pb = sortPoints p1' p2' p3'
    p1' = package p1
    p2' = package p2
    p3' = package p3
    package (x,y,z) = PWC $ (x,y,z,) $ fmap fromIntegral $ flip (calcLighting l) v $ normals M.! Triple x y z

scanLine _ s _ _ _ _ _ _  = error $ "Unsupported shading type: " ++ show s
{-# INLINE scanLine #-}

goroud :: PrimMonad m
       => PointWithColor -> PointWithColor -> PointWithColor
       -> Picture (PrimState m) -> m ()
goroud (PWC (!xb,!yb,!zb,!cb)) (PWC (!xm,!ym,!zm,!cm)) (PWC (!xt,!yt,!zt,!ct)) mArr = void $ do
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

flat :: PrimMonad m => Point -> Point -> Point -> Triple Double -> Picture (PrimState m) -> m ()
flat (P (!xb,!yb,!zb)) (P (!xm,!ym,!zm)) (P (!xt,!yt,!zt)) c mArr = void $
  forLoopState (round yb) (<= round yt) (+1) initState $ \(xl, zl, xr, zr, dxr, dzr, flipYet) y -> do
    let Pair (xl',zl') (xr',zr') = if xl < xr then Pair (xl,zl) (xr,zr) else Pair (xr,zr) (xl,zl)
    writeLine (floor xl') y zl' c (ceiling xr') y zr' c mArr
    if not flipYet && fromIntegral y + 1 >= ym
      then return (xl, zl, xm, zm, dxR2, dzR2, True)
      else return (xl + dxL, zl + dzL, xr + dxr, zr + dzr, dxr, dzr, flipYet)
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
{-# INLINE fSlope #-}

vSlope :: Triple Double -> Double -> Triple Double -> Double -> Triple Double
vSlope x0 y0 x1 y1 = (x1 - x0) / pure (y1 - y0)
{-# INLINE vSlope #-}

sortPoints :: Ord p => p -> p -> p -> Triple p
sortPoints p1 p2 p3 | p1 >= p2 && p1 >= p3 && p2 >= p3 = Triple p1 p2 p3
sortPoints p1 p2 p3 | p1 >= p2 && p1 >= p3           = Triple p1 p3 p2

sortPoints p1 p2 p3 | p2 >= p1 && p2 >= p3 && p1 >= p3 = Triple p2 p1 p3
sortPoints p1 p2 p3 | p2 >= p1 && p2 >= p3           = Triple p2 p3 p1

sortPoints p1 p2 p3 |                     p1 >= p2 = Triple p3 p1 p2
sortPoints p1 p2 p3                               = Triple p3 p2 p1
{-# SPECIALIZE INLINE sortPoints :: Point -> Point -> Point -> Triple Point #-}
{-# SPECIALIZE INLINE sortPoints :: PointWithColor -> PointWithColor -> PointWithColor -> Triple PointWithColor #-}
{-# SPECIALIZE INLINE sortPoints :: PointWithNormal -> PointWithNormal -> PointWithNormal -> Triple PointWithNormal #-}
