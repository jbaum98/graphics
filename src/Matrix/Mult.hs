{-# LANGUAGE FlexibleContexts, GADTs #-}

{-|
Module      : Matrix.Mult
Description : Provides fucntions for matrix multiplication.

Provides fucntions for matrix multiplication.
-}
module Matrix.Mult (matMult) where

import           Matrix.Base
import           Data.Array.Repa
import           Data.Array.Repa.Unsafe
import           Data.Array.Repa.Eval
import           Data.Array.Repa.Repr.Unboxed
import           Control.Monad.ST
import           Prelude hiding (zipWith)

-- |Multiplies two 'Matrix's using matrix multiplication
matMult :: (Num a, Unbox a, Elt a, Source r1 a, Source r2 a)
        => Matrix r1 a -> Matrix r2 a -> Matrix D a
{-# NOINLINE matMult #-}
matMult arr brr = delay $ runST $
  arr `deepSeqArray` brr `deepSeqArray` do
    trr <- computeUnboxedP $ trans brr
    let (Z :. h1 :. _) = extent arr
        (Z :. _ :. w2) = extent brr
        {-# INLINE f #-}
        f ix = sumAllS $ zipWith (*) (unsafeSlice arr (Any :. r :. All)) (unsafeSlice trr (Any :. c :. All))
          where
            (Z :. r :. c) = ix
    computeUnboxedP $ fromFunction (Z :. h1 :. w2) f

trans :: (Unbox a, Source r a)
      => Array r DIM2 a
      -> Array D DIM2 a
{-# NOINLINE trans #-}
trans arr = arr `deepSeqArray`
            unsafeBackpermute new_extent swap arr
  where
    swap (Z :. i :. j) = Z :. j :. i
    new_extent = swap (extent arr)
