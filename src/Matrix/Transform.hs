{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

{-|
Module      : Matrix.Transform
Description : Defines transformation 'Matrix's

-}

module Matrix.Transform (
    TransformMatrix,
    transMatrix,
    scaleMatrix,
    rotXMatrix,
    rotYMatrix,
    rotZMatrix,
    ) where

import           Matrix.Base
import           Matrix.Mult
import           Matrix.D3Point
import           Angle
import           Data.Array.Repa

-- |'Matrix's that apply transformations using matrix multiplication and
-- homogenous coordinates. To apply the transformation, matrix multiply the
-- preimage on the left by the 'TansformMatrix'. Because of matrix
-- multiplication's associativity, they can be composed themselves using matrix
-- multiplication and will be applied from right to left.
type TransformMatrix = Matrix D D3Coord

instance Monoid TransformMatrix where
  mempty = delay $ idMatrix 4
  mappend = matMult

-- |Create a translation 'Matrix'
transMatrix :: D3Point -> TransformMatrix
{-# INLINE transMatrix #-}
transMatrix (Triple x y z) = delay $ fromListUnboxed (Z :. 4 :. 4)
  [
    1, 0, 0, x,
    0, 1, 0, y,
    0, 0, 1, z,
    0, 0, 0, 1
  ]

-- |Create a scaling matrix that independently scales the x, y, and z
-- directions.
scaleMatrix :: D3Point -> TransformMatrix
{-# INLINE scaleMatrix #-}
scaleMatrix (Triple x y z) = delay $ fromListUnboxed (Z :. 4 :. 4)
  [
    x, 0, 0, 0,
    0, y, 0, 0,
    0, 0, z, 0,
    0, 0, 0, 1
  ]

-- |Create a rotation matrix that rotates clockwise about the x-axis when the
-- positive x-axis is pointing at you.
rotXMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
{-# INLINE rotXMatrix #-}
rotXMatrix = delay . rotXMatrixRad . degToRad
  where
    rotXMatrixRad theta = fromListUnboxed (Z :. 4 :. 4)
      [
        1, 0,          0,         0,
        0, cos theta, -sin theta, 0,
        0, sin theta,  cos theta, 0 ,
        0, 0,          0,         1
      ]

-- |Create a rotation matrix that rotates clockwise about the y-axis when the
-- positive y-axis is pointing at you.
rotYMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
{-# INLINE rotYMatrix #-}
rotYMatrix = delay . rotYMatrixRad . degToRad
  where
    rotYMatrixRad theta = fromListUnboxed (Z :. 4 :. 4)
      [
        cos theta, 0, sin theta, 0,
         0,         1, 0,         0,
        -sin theta, 0, cos theta, 0,
         0,         0, 0,         1
      ]

-- |Create a rotation matrix that rotates clockwise about the z-axis when the
-- positive z-axis is pointing at you.
rotZMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
{-# INLINE rotZMatrix #-}
rotZMatrix = delay . rotZMatrixRad . degToRad
  where
    rotZMatrixRad theta = fromListUnboxed (Z :. 4 :. 4)
      [
        cos theta, -sin theta, 0, 0,
        sin theta,  cos theta, 0, 0,
        0,          0,         1, 0,
        0,          0,         0, 1
      ]
