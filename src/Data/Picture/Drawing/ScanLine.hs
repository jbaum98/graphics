{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections #-}

module Data.Picture.Drawing.ScanLine (
  scanLine,
  -- * Individual shadings
  scanLineFlat,
  phong, goroud, flat,
  -- * Sorting points
  sortPoints,
  Point(..), PointWithNormal(..), PointWithColor(..)
  ) where

import Control.Monad

import Control.Monad.Primitive
import Control.Loop
import qualified Data.Map as M

import Data.Picture.Drawing.Lighting
import Data.Color
import Data.Pair
import Data.Picture.Drawing.Line (line)
import Data.Picture.Picture

-- | Fill a triangle with a single color
scanLineFlat :: PrimMonad m => (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
             -> Color -> Picture (PrimState m) -> m ()
scanLineFlat p1 p2 p3 = flat pb pm pt . fmap fromIntegral
  where
    Triple pt pm pb = sortPoints (P p1) (P p2) (P p3)

-- | Fill a triangle according to a 'ShadingType'
scanLine :: PrimMonad m
         => (Lighting, LightingConsts) -> ShadingType
         -> Triple Double          -- ^ view vector
         -> Triple Double          -- ^ normal vector
         -> VertexNormalMap
         -> (Double,Double,Double) -- ^ (x1,y1,z1)
         -> (Double,Double,Double) -- ^ (x2,y2,z2)
         -> (Double,Double,Double) -- ^ (x3,y3,z3)
         -> Picture (PrimState m) -> m ()

scanLine l Flat v n _ p1 p2 p3 = flat pb pm pt $ calcLighting l n v
  where
    Triple pt pm pb = sortPoints (P p1) (P p2) (P p3)

scanLine l Goroud v _ normals p1 p2 p3 = goroud pb pm pt
  where
    Triple pt pm pb = sortPoints p1' p2' p3'
    p1' = package p1
    p2' = package p2
    p3' = package p3
    package (x,y,z) = PWC (x,y,z,color)
      where color = calcLighting l n v
            n = normals M.! Triple x y z

scanLine l Phong v _ normals p1 p2 p3 = phong l v pb pm pt
  where
    Triple pt pm pb = sortPoints p1' p2' p3'
    p1' = package p1
    p2' = package p2
    p3' = package p3
    package (x,y,z) = PWN $ (x,y,z,n)
      where n = normals M.! Triple x y z

scanLine _ s _ _ _ _ _ _  = error $ "Unsupported shading type: " ++ show s
{-# INLINE scanLine #-}

newtype PointWithNormal = PWN (Double,Double,Double,Triple Double) deriving (Eq)

-- | Fill a triangle using Phong Shading
phong :: PrimMonad m
      => (Lighting, LightingConsts)
      -> Triple Double -- ^ view vector
      -> PointWithNormal -> PointWithNormal -> PointWithNormal
      -> Picture (PrimState m) -> m ()
phong l v (PWN (!xb,!yb,!zb,!nb)) (PWN (!xm,!ym,!zm,!nm)) (PWN (!xt,!yt,!zt,!nt)) mArr = void $ do
  forLoopState (round yb) (<= round yt) (+1) initState $ \((xl,zl,nl), (xr,zr,nr), (dxr,dzr,dnr), flipYet) y -> do
    let Pair (xl',zl',nl') (xr',zr',nr') = if xl < xr then Pair (xl,zl,nl) (xr,zr,nr) else Pair (xr,zr,nr) (xl,zl,nl)
        cl = light nl'
        cr = light nr'
    line (floor xl') y zl' cl (ceiling xr') y zr' cr mArr
    if not flipYet && fromIntegral y + 1 >= ym
      then return ((xl,zl,nl), (xm,zm,nm), (dxR2,dzR2,dnR2), True)
      else return ((xl + dxL, zl + dzL, nl + dnL), (xr + dxr, zr + dzr, nr + dnr), (dxr, dzr, dnr), flipYet)
  where
    initState = ((xb,zb,nb),(xb,zb,nb),(dxR1,dzR1,dnR1), False)
    dxL  = fSlope xb yb xt yt
    dzL  = fSlope zb yb zt yt
    dnL  = vSlope nb yb nt yt
    dxR1 = fSlope xb yb xm ym
    dzR1 = fSlope zb yb zm ym
    dnR1 = vSlope nb yb nm ym
    dxR2 = fSlope xm ym xt yt
    dzR2 = fSlope zm ym zt yt
    dnR2 = vSlope nm ym nt yt

    light = flip (calcLighting l) v

newtype PointWithColor = PWC (Double,Double,Double,Triple Double) deriving (Eq)

-- | Fill a triangle using Gouroud Shading
goroud :: PrimMonad m
       => PointWithColor -> PointWithColor -> PointWithColor
       -> Picture (PrimState m) -> m ()
goroud (PWC (!xb,!yb,!zb,!cb)) (PWC (!xm,!ym,!zm,!cm)) (PWC (!xt,!yt,!zt,!ct)) mArr = void $ do
  forLoopState (round yb) (<= round yt) (+1) initState $ \((xl,zl,cl), (xr,zr,cr), (dxr,dzr,dcr), flipYet) y -> do
    let Pair (xl',zl',cl') (xr',zr',cr') = if xl < xr then Pair (xl,zl,cl) (xr,zr,cr) else Pair (xr,zr,cr) (xl,zl,cl)
    line (floor xl') y zl' cl' (ceiling xr') y zr' cr' mArr
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

newtype Point = P (Double,Double,Double) deriving (Eq)

-- | Fill a triangle using Flat Shading
flat :: PrimMonad m => Point -> Point -> Point -> Triple Double -> Picture (PrimState m) -> m ()
flat (P (!xb,!yb,!zb)) (P (!xm,!ym,!zm)) (P (!xt,!yt,!zt)) c mArr = void $
  forLoopState (round yb) (<= round yt) (+1) initState $ \(xl, zl, xr, zr, dxr, dzr, flipYet) y -> do
    let Pair (xl',zl') (xr',zr') = if xl < xr then Pair (xl,zl) (xr,zr) else Pair (xr,zr) (xl,zl)
    line (floor xl') y zl' c (ceiling xr') y zr' c mArr
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

-- | Compute the slope between two points
fSlope :: Double -- ^ x1
       -> Double -- ^ y1
       -> Double -- ^ x2
       -> Double -- ^ y2
       -> Double
fSlope x0 y0 x1 y1 = (x1 - x0) / (y1 - y0)
{-# INLINE fSlope #-}

-- | Compute the slope between two points, but for vector valued points
vSlope :: Triple Double -- ^ x1
       -> Double        -- ^ y1
       -> Triple Double -- ^ x2
       -> Double        -- ^ y2
       -> Triple Double
vSlope x0 y0 x1 y1 = (x1 - x0) / pure (y1 - y0)
{-# INLINE vSlope #-}

instance Ord Point where
  compare (P (_,y1,_)) (P (_,y2,_)) = compare y1 y2

instance Ord PointWithColor where
  compare (PWC (_,y1,_,_)) (PWC (_,y2,_,_)) = compare y1 y2

instance Ord PointWithNormal where
  compare (PWN (_,y1,_,_)) (PWN (_,y2,_,_)) = compare y1 y2

-- | Sort the points from top to bottom
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
