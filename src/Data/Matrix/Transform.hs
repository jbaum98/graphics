{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}

{-|
Module      : Matrix.Transform
Description : Defines transformation 'Matrix's
-}

module Data.Matrix.Transform (
    TransformMatrix,
    idMatrix,
    transMatrix,
    scaleMatrix,
    rotXMatrix,
    rotYMatrix,
    rotZMatrix,
    transform,
    ) where

import Data.Color
import Data.Matrix.Base
import Data.Matrix.Mult
import Data.Matrix.Points

-- | 'Matrix's that apply transformations using matrix multiplication and
-- homogenous coordinates. To apply the transformation, matrix multiply the
-- preimage on the left by the 'TansformMatrix'. Because of matrix
-- multiplication's associativity, they can be composed themselves using matrix
-- multiplication and will be applied from right to left.
type TransformMatrix = Matrix Double

-- | Produces an 4 by 4 identity 'Matrix'
idMatrix :: TransformMatrix
idMatrix = fromLists [
  [ 1, 0, 0, 0 ],
  [ 0, 1, 0, 0 ],
  [ 0, 0, 1, 0 ],
  [ 0, 0, 0, 1 ]
  ]
{-# INLINE idMatrix #-}

-- | Create a translation 'Matrix'
transMatrix :: Triple Double -> TransformMatrix
transMatrix (Triple x y z) = fromLists [
  [ 1, 0, 0, x ],
  [ 0, 1, 0, y ],
  [ 0, 0, 1, z ],
  [ 0, 0, 0, 1 ]
  ]
{-# INLINE transMatrix #-}

-- | Create a scaling matrix that independently scales the x, y, and z
-- directions.
scaleMatrix :: Triple Double -> TransformMatrix
scaleMatrix (Triple x y z) = fromLists [
  [ x, 0, 0, 0 ],
  [ 0, y, 0, 0 ],
  [ 0, 0, z, 0 ],
  [ 0, 0, 0, 1 ]
  ]
{-# INLINE scaleMatrix #-}

-- | Create a rotation matrix that rotates clockwise about the x-axis when the
-- positive x-axis is pointing at you.
rotXMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
rotXMatrix = rotXMatrixRad . degToRad
  where
    rotXMatrixRad theta = fromLists [
      [ 1, 0,          0,         0 ],
      [ 0, cos theta, -sin theta, 0 ],
      [ 0, sin theta,  cos theta, 0 ],
      [ 0, 0,          0,         1 ]
      ]
{-# INLINE rotXMatrix #-}

-- | Create a rotation matrix that rotates clockwise about the y-axis when the
-- positive y-axis is pointing at you.
rotYMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
rotYMatrix = rotYMatrixRad . degToRad
  where
    rotYMatrixRad theta = fromLists [
      [ cos theta,  0, -sin theta, 0 ],
      [ 0,          1, 0,          0 ],
      [ sin theta,  0, cos theta,  0 ],
      [ 0,          0, 0,          1 ]
      ]
{-# INLINE rotYMatrix #-}

-- | Create a rotation matrix that rotates clockwise about the z-axis when the
-- positive z-axis is pointing at you.
rotZMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
rotZMatrix = rotZMatrixRad . degToRad
  where
    rotZMatrixRad theta = fromLists [
      [ cos theta, -sin theta, 0, 0 ],
      [ sin theta,  cos theta, 0, 0 ],
      [ 0,          0,         1, 0 ],
      [ 0,          0,         0, 1 ]
      ]
{-# INLINE rotZMatrix #-}

-- | Transform a single point
transform :: TransformMatrix -> Triple Double -> Triple Double
transform tm (Triple x y z) = flip get3DPoint 0 $ tm `matMult` pointMat
  where
    pointMat = fromList (4,1) [x, y, z, 1]

-- | Convert an angle from degrees to radians
degToRad :: (Num a, RealFrac a, Floating a) => a -> a
degToRad = (/ 180) . (* pi)
{-# SPECIALIZE INLINE degToRad :: Double -> Double #-}
