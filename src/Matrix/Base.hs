{-|
Module      : Matrix.Base
Description : Defines a basic Matrix type

Defines a basic Matrix type using REPA arrays.
-}

module Matrix.Base (
    Matrix(..),
    size,
    space,
    len,
    fromList,
    fromLists,
    fromFunction,
    (!),
    unsafeIndex,
    getCol,
    unsafeGetCol,
    getRow,
    unsafeGetRow,
    prettyMatrix,
    encode,
    decode,
    map
    ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Loop (numLoop)
import Prelude hiding (map)

data Matrix a = Matrix {
  rows :: {-# UNPACK #-} !Int,
  cols :: {-# UNPACK #-} !Int,
  lastIndex :: {-# UNPACK #-} !Int,
  vector :: V.Vector a
  }

size :: Matrix a -> (Int,Int)
{-# INLINE size #-}
size (Matrix r c _ _) = (r,c)

space :: MV.Unbox a => Matrix a -> Int
{-# INLINE space #-}
space m = V.length (vector m) - lastIndex m - 1

len :: MV.Unbox a => Matrix a -> Int
{-# INLINE len #-}
len = V.length . vector

instance (Show a, MV.Unbox a) => Show (Matrix a) where
  show (Matrix r c l v) = "fromList (" ++ show r ++ "," ++ show c ++ ") " ++ show l ++ " " ++ show v

encode :: (Int,Int) -> (Int,Int) -> Int
{-# INLINE encode #-}
encode (rs,_) (i,j) = (j-1)*rs + i - 1

decode :: (Int,Int) -> Int -> (Int,Int)
{-# INLINE decode #-}
decode (rs,_) k = (r+1,q+1)
 where
  (q,r) = quotRem k rs

listLen :: (Int,Int) -> Int
{-# INLINE listLen #-}
listLen (r,c) = r * c

inRange :: (Int,Int) -> (Int,Int) -> Bool
{-# INLINE inRange #-}
(r,c) `inRange` (rs, cs) = r <= rs &&
                           c <= cs &&
                           r > 0  &&
                           c > 0

fromList :: MV.Unbox a => (Int,Int) -> [a] -> Matrix a
{-# INLINE fromList #-}
fromList s xs = uncurry Matrix s (listLen s - 1) (V.fromListN (listLen s) xs)

fromFunction :: MV.Unbox a => (Int,Int) -> ((Int,Int) -> a) -> Matrix a
{-# INLINE fromFunction #-}
fromFunction s@(rs,cs) f = Matrix rs cs (listLen s - 1) $ V.create $ do
  v <- MV.new $ rs * cs
  let en = encode s
  numLoop 1 rs $
    \i -> numLoop 1 cs $
    \j -> MV.unsafeWrite v (en (i,j)) (f (i,j))
  return v

fromLists :: MV.Unbox a => [[a]] -> Matrix a
{-# INLINE fromLists #-}
fromLists xss = fromFunction (r,c) $ \(i,j) -> (xss !! (i - 1)) !! (j - 1)
  where
    r = length xss
    c = length $ head xss

unsafeIndex, (!) :: MV.Unbox a => Matrix a -> (Int,Int) -> a
{-# INLINE unsafeIndex #-}
(Matrix r c _ xs) `unsafeIndex` i = xs `V.unsafeIndex` encode (r,c) i
{-# INLINE (!) #-}
(!) m i | i `inRange` size m = m `unsafeIndex` i
        | otherwise = error $ "The provided index (" ++ show i ++ ") is out of bounds for a matrix of size " ++ showSize (size m)

showSize :: (Int,Int) -> String
showSize (r,c) = show r ++ "x" ++ show c

unsafeGetCol, getCol :: MV.Unbox a => Int -> Matrix a -> V.Vector a
{-# INLINE unsafeGetCol #-}
{-# INLINE getCol #-}
unsafeGetCol j (Matrix r _ _ v) = V.unsafeSlice ((j-1) * r) r v
getCol j m | j <= cols m && j > 0 = unsafeGetCol j m
           | otherwise = error $ "getCol: column index " ++ show j ++ "out of bounds for matrix with " ++ show (cols m) ++ " columns"

unsafeGetRow, getRow :: MV.Unbox a => Int -> Matrix a -> V.Vector a
{-# INLINE unsafeGetRow #-}
{-# INLINE getRow #-}
unsafeGetRow i m = V.generate (cols m) $ \j -> vector m `V.unsafeIndex` encode (size m) (i,j+1)
getRow i m | i <= rows m = unsafeGetRow i m
           | otherwise = error $ "getRow: row index " ++ show i ++ "out of bounds for matrix with " ++ show (rows m) ++ " rows"

map :: (MV.Unbox a, MV.Unbox b) => (a -> b) -> Matrix a -> Matrix b
map f m = m { vector = V.map f $ vector m }

-- |Prints a 'Matrix' in rows and columns.
--  Doesn't align elements if they are too long.
prettyMatrix :: (Show a, MV.Unbox a) => Matrix a -> String
prettyMatrix (Matrix rs cs _ xs) =  unlines [unwords $ fmap (showEl r) [1..cs] | r <- [1..rs]]
  where
    showEl r c = fill . show $ xs V.! encode (rs,cs) (r,c)
    fill str = replicate (maxLen - length str) ' ' ++ str
    maxLen = V.maximum $ V.map (length . show) xs
