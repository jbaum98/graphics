{-# LANGUAGE FlexibleContexts, GADTs, BangPatterns, MagicHash, UnboxedTuples #-}

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
import GHC.Conc
import GHC.Prim
import GHC.Exts
import Debug.Trace

-- help from https://hackage.haskell.org/package/matrix-0.3.4.4/docs/src/Data-Matrix.html

-- |Multiplies two 'Matrix's using matrix multiplication
matMult, unsafeMatMult :: Matrix Double -> Matrix Double -> Matrix Double
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
              !s = case a1 of { (D# a1') ->
                   case b1 of { (D# b1') ->
                   case a2 of { (D# a2') ->
                   case b2 of { (D# b2') ->
                   case a3 of { (D# a3') ->
                   case b3 of { (D# b3') ->
                   case a4 of { (D# a4') ->
                   case b4 of { (D# b4') ->
                              D# ( (a1' *## b1') +## (a2' *## b2') +## (a3' *## b3') +## (a4' *## b4'))
                  }}}}}}}}
          MV.unsafeWrite out (fb + r) s
        return out
