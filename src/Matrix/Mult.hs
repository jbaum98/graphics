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
matMult, unsafeMatMult :: (MV.Unbox a, Num a, PrimMonad m) => Matrix a -> Matrix a -> m (Matrix a)
matMult a b | cols a == rows b = unsafeMatMult a b
            | otherwise = error "The number of columns of the first matrix must equal the number of rows of the second matrix"

unsafeMatMult a b = do
  tmp <- MV.new 4
  b' <- V.unsafeThaw (vector b)
  let getA = V.unsafeIndex (vector a) . encode (size a)
      getTmp = MV.unsafeRead tmp
  numLoop 1 (cols b) $ \c -> do
    V'.unsafeCopy tmp $ unsafeGetCol c b
    numLoop 1 4 $ \r -> do
      t0 <- getTmp 0
      t1 <- getTmp 1
      t2 <- getTmp 2
      t3 <- getTmp 3
      MV.unsafeWrite b' (encode (size b) (r,c)) $
        getA (r,1) * t0 +
        getA (r,2) * t1 +
        getA (r,3) * t2 +
        getA (r,4) * t3
  newB <- V.unsafeFreeze b'
  return $ b { vector = newB }
