{-# LANGUAGE FlexibleContexts, GADTs #-}

{-|
Module      : Matrix.Mult
Description : Provides fucntions for matrix multiplication.

Provides fucntions for matrix multiplication.
-}
module Matrix.Mult (matMult) where

import Matrix.Base
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as V'
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Loop
import Control.Monad.Primitive

-- help from https://hackage.haskell.org/package/matrix-0.3.4.4/docs/src/Data-Matrix.html

-- |Multiplies two 'Matrix's using matrix multiplication
matMult, unsafeMatMult :: (MV.Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
matMult a b | cols a == rows b = unsafeMatMult a b
            | otherwise = error "The number of columns of the first matrix must equal the number of rows of the second matrix"

unsafeMatMult a b = b { vector = newB }
  where
    newB = V.create $ do
        out <- {-# SCC "alloc_new" #-} MV.new (len b)
        numLoop 1 (cols b) $ \c ->
          numLoop 1 4 $ \r ->
           MV.unsafeWrite out (encode (size b) (r,c)) $ {-# SCC "mult" #-}
             a `unsafeIndex` (r,1) * b `unsafeIndex` (1,c) +
             a `unsafeIndex` (r,2) * b `unsafeIndex` (2,c) +
             a `unsafeIndex` (r,3) * b `unsafeIndex` (3,c) +
             a `unsafeIndex` (r,4) * b `unsafeIndex` (4,c)
        return out
