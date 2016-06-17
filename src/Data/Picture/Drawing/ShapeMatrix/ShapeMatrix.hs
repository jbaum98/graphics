module Data.Picture.Drawing.ShapeMatrix.ShapeMatrix (
  ShapeMatrix(..),
  ) where


import Control.Monad.Primitive

import Data.Color
import Data.Matrix
import Data.Picture.Drawing.Lighting
import Data.Picture.Picture

-- | A 'ShapeMatrix' is a matrix that represents something that can be drawn
class ShapeMatrix d where
  -- | Draw the shape in a given color
  drawColor :: PrimMonad m => Color -> (Lighting,LightingConsts) -> ShadingType -> d -> Picture (PrimState m) -> m ()

  -- | Draw a shape
  draw :: PrimMonad m => (Lighting,LightingConsts) -> ShadingType -> d -> Picture (PrimState m) -> m ()
  draw = drawColor white

  -- | Convert to a plain matrix
  unwrap :: d -> Matrix Double

  -- | Convert a plain matrix to this type of 'ShapeMatrix'
  wrap :: Matrix Double -> d

  -- | Lift a function that applies to 'Matrix's to 'ShapeMatrix's
  liftDraw :: (Matrix Double -> Matrix Double) -> d -> d
  liftDraw f = wrap . f . unwrap

  -- | Lift a binary function that applies to 'Matrix's to 'ShapeMatrix's
  liftDraw2 :: (Matrix Double -> Matrix Double -> Matrix Double) -> d -> d -> d
  liftDraw2 f d1 d2 = wrap $ f (unwrap d1) (unwrap d2)

  -- | Multiply a plain matrix by a 'ShapeMatrix'
  matMultD :: Matrix Double -> d -> d
  matMultD = liftDraw . matMult
