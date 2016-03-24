{-# LANGUAGE FlexibleContexts #-}

module Matrix.Transform (
  transMatrix, scaleMatrix, rotXMatrix, rotYMatrix, rotZMatrix,
  ) where

import Matrix.Base
import Matrix.D3Point
import Angle
import Data.Array.Repa

type Coord = D3Coord
type Point = D3Point

transMatrix :: Point -> Matrix U Coord
{-# INLINE transMatrix #-}
transMatrix (Triple x y z) = fromListUnboxed (Z:. 4 :. 4) [
  1, 0, 0, x,
  0, 1, 0, y,
  0, 0, 1, z,
  0, 0, 0, 1
  ]

scaleMatrix :: Point -> Matrix U Coord
{-# INLINE scaleMatrix #-}
scaleMatrix (Triple x y z) = fromListUnboxed (Z:. 4 :. 4) [
  x, 0, 0, 0,
  0, y, 0, 0,
  0, 0, z, 0,
  0, 0, 0, 1
  ]

rotXMatrix :: Double -> Matrix U Coord
{-# INLINE rotXMatrix #-}
rotXMatrix = rotXMatrixRad . degToRad
  where
    rotXMatrixRad theta = fromListUnboxed (Z:. 4 :. 4) [
      1, 0,         0,          0,
      0, cos theta, -sin theta, 0,
      0, sin theta, cos theta,  0,
      0, 0,         0,          1
      ]

rotYMatrix :: Double -> Matrix U Coord
{-# INLINE rotYMatrix #-}
rotYMatrix = rotYMatrixRad . degToRad
  where
    rotYMatrixRad theta = fromListUnboxed (Z:. 4 :. 4) [
      cos theta,  0, sin theta, 0,
      0,          1, 0,         0,
      -sin theta, 0, cos theta, 0,
      0,          0, 0,         1
      ]

rotZMatrix :: Double -> Matrix U Coord
{-# INLINE rotZMatrix #-}
rotZMatrix = rotZMatrixRad . degToRad
  where
    rotZMatrixRad theta = fromListUnboxed (Z:. 4 :. 4) [
      cos theta, -sin theta, 0, 0,
      sin theta, cos theta,  0, 0,
      0,         0,          1, 0,
      0,         0,           0, 1
      ]
