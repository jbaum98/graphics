module Data.Matrix.Points (
    empty,
    emptyWith,
    addP,
    growMat,
    mergeCols,
    get2DPoint,
    get3DPoint
  ) where

import Control.Monad.ST

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Loop (numLoop)

import Data.Pair
import Data.Matrix.Base

-- |Construct an empty 4x0 'Matrix'
empty :: MV.Unbox a => Matrix a
empty = Matrix 4 0 (-1) $ V.create $ MV.new 400
{-# INLINE empty #-}

-- |Construct an empty 4x0 'Matrix' with a given number of spaces
emptyWith :: MV.Unbox a => Int -> Matrix a
emptyWith n = Matrix 4 0 (-1) $ V.create $ MV.new n
{-# INLINE emptyWith #-}

-- |Add a point to a 4xn 'Matrix', growing the 'Matrix' if necessary. This
-- mutates the 'Matrix' and is probably a bad idea, but is this way for
-- performance reasons and because you rarely access the old version. Therefore
-- you should /not/ access the old version.
addP :: (MV.Unbox a, Num a) => Triple a -> Matrix a -> Matrix a
addP p@(Triple x y z) m@(Matrix r c l v)
  | space m >= 4 = Matrix r (c+1) (l + 4) (runST $ V.unsafeThaw v >>= addAction)
  | otherwise = addP p $ growMat m 1000
  where
    addAction w = set w 1 x >> set w 2 y >> set w 3 z >> set w 4 1 >>
                  V.unsafeFreeze w
    set w n = MV.write w (encode (r,c+1) (n,c+1))
{-# SPECIALIZE addP :: Triple Double -> Matrix Double -> Matrix Double #-}

-- |Grow a 'Matrix' to fit additional elements
growMat :: MV.Unbox a => Matrix a -> Int -> Matrix a
growMat m n = m { vector = newV (vector m) }
  where
    newV v = runST $ do
      v' <- V.unsafeThaw v
      v'' <- MV.grow v' n
      V.unsafeFreeze v''

-- |Merge two 'Matrix's by appending the columns of one to another.
mergeCols :: MV.Unbox a => Matrix a -> Matrix a -> Matrix a
mergeCols b a
  | rows a  /= rows b = error "The two matrices have a different number of rows"
  | otherwise = runST $ do
      b' <- V.unsafeThaw $ vector b
      a' <- if space a < lastIndex b + 1
            then V.unsafeThaw (vector a) >>= \w ->
              MV.unsafeGrow w (lastIndex b + 1 - space a)
            else V.unsafeThaw (vector a)
      let newLast = lastIndex a + lastIndex b + 1
      numLoop 0 (lastIndex b) $
        \i -> do
          el <- MV.unsafeRead b' i
          MV.unsafeWrite a' (lastIndex a + i + 1) el
      newV <- V.unsafeFreeze a'
      return $ Matrix (rows a) (cols a + cols b) newLast newV

-- |Extract the /n/th 2D point from a 'Matrix'
get2DPoint :: Matrix Double -> Int -> Pair Int
get2DPoint m n = round <$> Pair x y
  where
    x = col `V.unsafeIndex` 0
    {-# INLINE x #-}
    y = col `V.unsafeIndex` 1
    {-# INLINE y #-}
    col = unsafeGetCol (n+1) m
{-# INLINE get2DPoint #-}

-- |Extract the /n/th 3D point from a 'Matrix'
get3DPoint :: Matrix Double -> Int -> Triple Double
get3DPoint m n = Triple x y z
  where
    x = col `V.unsafeIndex` 0
    {-# INLINE x #-}
    y = col `V.unsafeIndex` 1
    {-# INLINE y #-}
    z = col `V.unsafeIndex` 2
    {-# INLINE z #-}
    col = unsafeGetCol (n+1) m
{-# INLINE get3DPoint #-}
