{-# LANGUAGE RankNTypes #-}

module Matrix.EdgeMatrix (
    EdgeMatrix,
    -- ** Point Representation
    module Matrix.D3Point,
    -- ** Construction
    empty,
    fromPoints,
--   connectPoints,
--   point,
    edge,
    addEdge,
    addPoint,
    mergeCols,
    -- ** Retrieving 'Point's
   drawLinesColor,
   drawLines,
    ) where

import           Matrix.Base
import           Matrix.D3Point
import Pair
import D2Point
import Picture
import Utils
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Loop (numLoop)
import Debug.Trace

type EdgeMatrix = Matrix D3Coord

empty :: EdgeMatrix
empty = Matrix 4 0 (-1) $ V.create $ MV.new 400

addPoint :: PrimMonad m => D3Point -> EdgeMatrix -> m EdgeMatrix
addPoint p@(Triple x y z) m@(Matrix r c l v) | space m >= 4 = Matrix r (c+1) (l + 4) <$> newV
                                             | otherwise = addPoint p $ runST $ growMat m 400
  where
    newV = do
      v' <- V.unsafeThaw v
      set v' 1 x >> set v' 2 y >> set v' 3 z >> set v' 4 1
      V.unsafeFreeze v'
    set w n = MV.write w (encode (r,c+1) (n,c+1))

growMat :: (PrimMonad m, MV.Unbox a) => Matrix a -> Int -> m (Matrix a)
growMat m n = do
  let v = vector m
  v' <- V.unsafeThaw v
  v'' <- MV.grow v' n
  newV <- V.unsafeFreeze v''
  return $ m { vector = newV }

mergeCols :: (MV.Unbox a, PrimMonad m) => Matrix a -> Matrix a -> m (Matrix a)
mergeCols a b | rows a  /= rows b = error "The two matrices have a different number of rows"
              | otherwise = do
                  b' <- V.unsafeThaw $ vector b
                  a' <- if space a < lastIndex b + 1
                       then V.unsafeThaw (vector a) >>= \w -> MV.unsafeGrow w (lastIndex b + 1 - space a)
                       else V.unsafeThaw (vector a)
                  let newLast = lastIndex a + lastIndex b + 1
                  numLoop 0 (lastIndex b) $
                    \i -> do
                      el <- MV.unsafeRead b' i
                      MV.unsafeWrite a' (lastIndex a + i + 1) el
                  newV <- V.unsafeFreeze a'
                  return $ Matrix (rows a) (cols a + cols b) newLast newV

fromPoints :: [D3Point] -> EdgeMatrix
fromPoints points = Matrix 4 (length points) 0 $ V.generate (length points * 4) f
  where f i = cIndex cN $ points !! pN
          where (pN, cN) = i `quotRem` 4
        cIndex 0 (Triple x _ _) = x
        cIndex 1 (Triple _ y _) = y
        cIndex 2 (Triple _ _ z) = z
        cIndex 3 _ = 1

edge :: D3Point -> D3Point -> EdgeMatrix
edge (Triple x1 y1 z1) (Triple x2 y2 z2) = Matrix 4 2 7 $
  V.fromList [x1, y1, z1, 1, x2, y2, z2, 1]

-- |Adds an edge conecting to 'D3Point's to an 'EdgeMatrix'
addEdge :: PrimMonad m => D3Point -> D3Point -> EdgeMatrix -> m EdgeMatrix
addEdge p1 p2 = addPoint p1 >=> addPoint p2

-- |Gets the @n@th point from an 'EdgeMatrix' as a 'D2Point', dropping the
-- z-coordinate

getPoint :: EdgeMatrix -> Int -> D2Point
getPoint m n = round <$> Pair x y
  where
    x = col `V.unsafeIndex` 0
    y = col `V.unsafeIndex` 1
    col = unsafeGetCol (n+1) m

-- |Draws all lines specified by an 'EdgeMatrix' in a 'Color'
drawLinesColor :: Color -> EdgeMatrix -> Picture -> Picture
drawLinesColor color m = compose [drawColorLine color (getPoint m i) (getPoint m (i+1)) | i <- [0,2.. cols m - 1]]

-- |Draws all lines specified by an 'EdgeMatrix' in a white
drawLines :: EdgeMatrix -> Picture -> Picture
drawLines = drawLinesColor white
