{-# LANGUAGE FlexibleContexts, GADTs, BangPatterns, MagicHash #-}

{-|
Module      : Matrix.Mult
Description : Provides fucntions for matrix multiplication.

Provides fucntions for matrix multiplication.
-}
module Data.Matrix.Mult (matMult) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.Loop

import Data.Matrix.Base
-- help from https://hackage.haskell.org/package/matrix-0.3.4.4/docs/src/Data-Matrix.html

-- |Multiplies two 'Matrix's using matrix multiplication
matMult, unsafeMatMult :: (Num a, MV.Unbox a) => Matrix a -> Matrix a -> Matrix a
matMult a b | cols a == rows b = unsafeMatMult a b
            | otherwise = error "The number of columns of the first matrix must equal the number of rows of the second matrix"

unsafeMatMult a b = b { vector = newB }
  where
    newB = V.create $ do
        out <- {-# SCC "alloc_new" #-} MV.new (len b)
        let ra = rows a
            rb = rows b
        numLoop 0 (cols b - 1) $ \c ->
          numLoop 0 3 $ \r -> do
          let !a' = vector a
              !b' = vector  b
              fa = ra + r
              fb = c * rb
              !a1 = a' `V.unsafeIndex` r
              !b1 = b' `V.unsafeIndex` fb
              !a2 = a' `V.unsafeIndex` fa
              !b2 = b' `V.unsafeIndex` (fb + 1)
              !a3 = a' `V.unsafeIndex` (fa + ra)
              !b3 = b' `V.unsafeIndex` (fb + 2)
              !a4 = a' `V.unsafeIndex` (fa + ra + ra)
              !b4 = b' `V.unsafeIndex` (fb + 3)
          MV.unsafeWrite out (fb + r) $ (a1 * b1) + (a2 * b2) + (a3 * b3) + (a4 * b4)
        return out

{-# SPECIALIZE matMult :: Matrix Double -> Matrix Double -> Matrix Double #-}
{-# SPECIALIZE unsafeMatMult :: Matrix Double -> Matrix Double -> Matrix Double #-}

{-# SPECIALIZE matMult :: Matrix Int -> Matrix Int -> Matrix Int #-}
{-# SPECIALIZE unsafeMatMult :: Matrix Int -> Matrix Int -> Matrix Int #-}
