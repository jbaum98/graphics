{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}

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
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Loop
import Debug.Trace

-- |'Matrix's that apply transformations using matrix multiplication and
-- homogenous coordinates. To apply the transformation, matrix multiply the
-- preimage on the left by the 'TansformMatrix'. Because of matrix
-- multiplication's associativity, they can be composed themselves using matrix
-- multiplication and will be applied from right to left.
type TransformMatrix = Matrix D3Coord

-- |Produces an 4 by 4 identity 'Matrix'
idMatrix :: TransformMatrix
{-# INLINE idMatrix #-}
idMatrix = fromLists [
  [ 1, 0, 0, 0 ],
  [ 0, 1, 0, 0 ],
  [ 0, 0, 1, 0 ],
  [ 0, 0, 0, 1 ]
  ]

-- |Create a translation 'Matrix'
transMatrix :: D3Point -> TransformMatrix
{-# INLINE transMatrix #-}
transMatrix (Triple x y z) = fromLists [
  [ 1, 0, 0, x ],
  [ 0, 1, 0, y ],
  [ 0, 0, 1, z ],
  [ 0, 0, 0, 1 ]
  ]

-- |Create a scaling matrix that independently scales the x, y, and z
-- directions.
scaleMatrix :: D3Point -> TransformMatrix
{-# INLINE scaleMatrix #-}
scaleMatrix (Triple x y z) = fromLists [
  [ x, 0, 0, 0 ],
  [ 0, y, 0, 0 ],
  [ 0, 0, z, 0 ],
  [ 0, 0, 0, 1 ]
  ]

-- |Create a rotation matrix that rotates clockwise about the x-axis when the
-- positive x-axis is pointing at you.
rotXMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
{-# INLINE rotXMatrix #-}
rotXMatrix = rotXMatrixRad . degToRad
  where
    rotXMatrixRad theta = fromLists [
      [ 1, 0,          0,         0 ],
      [ 0, cos theta, -sin theta, 0 ],
      [ 0, sin theta,  cos theta, 0 ],
      [ 0, 0,          0,         1 ]
      ]

-- |Create a rotation matrix that rotates clockwise about the y-axis when the
-- positive y-axis is pointing at you.
rotYMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
{-# INLINE rotYMatrix #-}
rotYMatrix = rotYMatrixRad . degToRad
  where
    rotYMatrixRad theta = fromLists [
      [ cos theta,  0, sin theta, 0 ],
      [  0,         1, 0,         0 ],
      [ -sin theta, 0, cos theta, 0 ],
      [  0,         0, 0,         1 ]
      ]

-- |Create a rotation matrix that rotates clockwise about the z-axis when the
-- positive z-axis is pointing at you.
rotZMatrix :: Double -- ^The angle of rotation in degrees
           -> TransformMatrix
{-# INLINE rotZMatrix #-}
rotZMatrix = rotZMatrixRad . degToRad
  where
    rotZMatrixRad theta = fromLists [
      [ cos theta, -sin theta, 0, 0 ],
      [ sin theta,  cos theta, 0, 0 ],
      [ 0,          0,         1, 0 ],
      [ 0,          0,         0, 1 ]
      ]

progress :: PrimMonad m => [TransformMatrix] -> EdgeMatrix -> m EdgeMatrix
progress ts e = do
  (_,eFinal) <- numLoopState 1 (length ts) (ts,empty) $ \(t:ts',e') _ -> do
    thisE <- t `matMult` e
    appended <- e' `mergeCols` thisE
    return (ts', appended)
  return eFinal

drawProgress :: PrimMonad m => [TransformMatrix] -> EdgeMatrix -> Picture -> m Picture
drawProgress ts em p = flip drawLines p <$> progress ts em

drawProgressColors :: PrimMonad m => [(TransformMatrix, Color)] -> EdgeMatrix -> Picture -> m Picture
drawProgressColors tcs e p = do
  (_,pFinal) <- numLoopState 1 (length tcs) (tcs,p) $ \((t,color):tcs',p') _ -> do
    newE <- t `matMult` e
    let !newP = drawLinesColor color newE p'
    return (tcs', newP)
  return pFinal
