module Matrix.ShapeMatrix (
  ShapeMatrix,
  drawColor,
  draw,
  unwrap,
  wrap,
  liftDraw,
  liftDraw2,
  matMultD
  ) where

import Picture
import Matrix.Base
import Matrix.D3Point
import Matrix.Mult

class ShapeMatrix d where
  drawColor :: Color -> d -> Picture -> Picture

  draw :: d -> Picture -> Picture
  draw = drawColor white

  unwrap :: d -> Matrix D3Coord
  wrap :: Matrix D3Coord -> d

  liftDraw :: (Matrix D3Coord -> Matrix D3Coord) -> d -> d
  liftDraw f = wrap . f . unwrap

  liftDraw2 :: (Matrix D3Coord -> Matrix D3Coord -> Matrix D3Coord) -> d -> d -> d
  liftDraw2 f d1 d2 = wrap $ f (unwrap d1) (unwrap d2)

  matMultD :: Matrix D3Coord -> d -> d
  matMultD = liftDraw . matMult
