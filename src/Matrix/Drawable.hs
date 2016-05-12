module Matrix.Drawable (
  Drawable,
  drawColor,
  draw,
  run,
  wrap,
  liftDraw,
  matMultD
  ) where

import Picture
import Matrix.Base
import Matrix.D3Point
import Matrix.Mult

class Drawable d where
  drawColor :: Color -> d -> Picture -> Picture

  draw :: d -> Picture -> Picture
  draw = drawColor white

  run :: d -> Matrix D3Coord
  wrap :: Matrix D3Coord -> d

  liftDraw :: (Matrix D3Coord -> Matrix D3Coord) -> d -> d
  liftDraw f = wrap . f .run

  matMultD :: Matrix D3Coord -> d -> d
  matMultD = liftDraw . matMult
