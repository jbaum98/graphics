module Data.Matrix.ShapeMatrix (
  ShapeMatrix,
  drawColor,
  draw,
  unwrap,
  wrap,
  liftDraw,
  liftDraw2,
  matMultD
  ) where

import Data.Color
import Data.D3Point
import Data.Matrix.Base
import Data.Matrix.Mult
import Data.Picture.Picture
import Control.Monad.Primitive
import Data.Lighting

class ShapeMatrix d where
  drawColor :: PrimMonad m => Color -> Lighting -> d -> Picture (PrimState m) -> m ()

  draw :: PrimMonad m => Lighting -> d -> Picture (PrimState m) -> m ()
  draw = drawColor white

  unwrap :: d -> Matrix D3Coord
  wrap :: Matrix D3Coord -> d

  liftDraw :: (Matrix D3Coord -> Matrix D3Coord) -> d -> d
  liftDraw f = wrap . f . unwrap

  liftDraw2 :: (Matrix D3Coord -> Matrix D3Coord -> Matrix D3Coord) -> d -> d -> d
  liftDraw2 f d1 d2 = wrap $ f (unwrap d1) (unwrap d2)

  matMultD :: Matrix D3Coord -> d -> d
  matMultD = liftDraw . matMult
