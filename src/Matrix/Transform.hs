{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

{-|
Module      : Matrix.Transform
Description : Defines transformation 'Matrix's

-}

module Matrix.Transform (
    TransformMatrix,
    idMatrix,
    transMatrix,
    scaleMatrix,
    rotXMatrix,
    rotYMatrix,
    rotZMatrix,
    progress,
    drawProgress,
    drawProgressColors
    ) where

import           Matrix.Base
import           Matrix.Mult
import           Matrix.D3Point
import           Matrix.EdgeMatrix
import           Picture
import           Angle
import           Utils
import           Data.Array.Repa

-- |'Matrix's that apply transformations using matrix multiplication and
-- homogenous coordinates. To apply the transformation, matrix multiply the
-- preimage on the left by the 'TansformMatrix'. Because of matrix
-- multiplication's associativity, they can be composed themselves using matrix
-- multiplication and will be applied from right to left.
type TransformMatrix = Matrix D D3Coord

-- |Produces an 4 by 4 identity 'Matrix'
idMatrix :: TransformMatrix
{-# INLINE idMatrix #-}
idMatrix = delay $ fromListUnboxed (Z :. 4 :. 4)
  [
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  ]

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

progress' :: (a -> a -> a) -> [a] -> [a]
progress' _ [] = []
progress' _ [t] = [t]
progress' f (t1:ts) = t1 : fmap (f t1) (progress' f ts)

progress :: [TransformMatrix] -> EdgeMatrix -> EdgeMatrix
progress ts e = foldl (Data.Array.Repa.++) empty . fmap (`matMult` e) . progress' (flip matMult) $ ts
  where
    empty = delay $ fromListUnboxed (ix2 4 0) []

drawProgress :: [TransformMatrix] -> EdgeMatrix -> Picture -> Picture
drawProgress ts = drawLines . progress ts

drawProgressColors :: [(TransformMatrix, Color)] -> EdgeMatrix -> Picture -> Picture
drawProgressColors tcs e = compose $ fmap (\(ls, color) -> drawLinesColor color ls) edges
  where
    edges = fmap (\(t, color) -> (t `matMult` e, color)) . progress' (\(t1, _) (t2, color) -> (t2 `matMult` t1, color)) $ tcs
