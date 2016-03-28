{-# LANGUAGE FlexibleContexts, GADTs #-}

{-|
Module      : Matrix.Mult
Description : Provides fucntions for matrix multiplication.

Provides fucntions for matrix multiplication.
-}

module Matrix.Mult (idMatrix, matMult) where

import           Matrix.Base
import           Data.Array.Repa
import           Data.Array.Repa.Unsafe
import           Data.Array.Repa.Eval
import           Data.Array.Repa.Repr.Unboxed
import           Prelude hiding ()

-- |Produces an @n@ by @n@ identity 'Matrix'
idMatrix :: (Num a, Unbox a) => Int -> Matrix U a
idMatrix n = computeUnboxedS $ fromFunction (ix2 n n) f
  where
    f (Z :. r :. c)
      | r == c = 1
      | otherwise = 0

-- |Multiplies two 'Matrix's using matrix multiplication
matMult :: (Num a, Unbox a, Elt a, Source r1 a, Source r2 a)
        => Matrix r1 a -> Matrix r2 a -> Matrix D a
{-# NOINLINE matMult #-}
matMult arr brr = arr `deepSeqArray` brr `deepSeqArray`
                  fromFunction (Z :. r :. c) dotRowCol
  where
    trr = transposeMat brr
    dotRowCol (Z :. row :. col) = sumAllS
      $ unsafeSlice arr (Any :. row :. All) *^ unsafeSlice trr (Any :. col :. All)
    (Z :. r :. _) = extent arr
    (Z :. _ :. c) = extent brr

-- |Transposes a 'Matrix' by switching the rows and the columns
transposeMat :: (Unbox a, Source r a) => Matrix r a -> Matrix D a
{-# NOINLINE transposeMat #-}
transposeMat mat = mat `deepSeqArray` backpermute new_extent swap mat
  where
    swap (Z :. i :. j) = Z :. j :. i
    new_extent = swap (extent mat)
