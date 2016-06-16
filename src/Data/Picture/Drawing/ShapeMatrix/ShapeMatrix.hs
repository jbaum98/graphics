module Data.Picture.Drawing.ShapeMatrix.ShapeMatrix (
  ShapeMatrix,
  drawColor,
  draw,
  unwrap,
  wrap,
  liftDraw,
  liftDraw2,
  matMultD
  ) where


import Control.Monad.Primitive

import Data.Color
import Data.Matrix
import Data.Picture.Drawing.Lighting
import Data.Picture.Picture

class ShapeMatrix d where
  drawColor :: PrimMonad m => Color -> (Lighting,LightingConsts) -> ShadingType -> d -> Picture (PrimState m) -> m ()

  draw :: PrimMonad m => (Lighting,LightingConsts) -> ShadingType -> d -> Picture (PrimState m) -> m ()
  draw = drawColor white

  unwrap :: d -> Matrix Double
  wrap :: Matrix Double -> d

  liftDraw :: (Matrix Double -> Matrix Double) -> d -> d
  liftDraw f = wrap . f . unwrap

  liftDraw2 :: (Matrix Double -> Matrix Double -> Matrix Double) -> d -> d -> d
  liftDraw2 f d1 d2 = wrap $ f (unwrap d1) (unwrap d2)

  matMultD :: Matrix Double -> d -> d
  matMultD = liftDraw . matMult
