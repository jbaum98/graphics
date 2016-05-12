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
    empty,
    emptyWith,
    addP,
    growMat,
    mergeCols,
    getD2Point,
    getD3Point
    ) where

import Control.Monad.ST
import Prelude hiding (map)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Loop (numLoop)

import Pair
import Matrix.D3Point
import D2Point

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

empty :: MV.Unbox a => Matrix a
empty = Matrix 4 0 (-1) $ V.create $ MV.new 400

emptyWith :: MV.Unbox a => Int -> Matrix a
emptyWith n = Matrix 4 0 (-1) $ V.create $ MV.new n

-- |Prints a 'Matrix' in rows and columns.
--  Doesn't align elements if they are too long.
prettyMatrix :: (Show a, MV.Unbox a) => Matrix a -> String
prettyMatrix (Matrix rs cs _ xs) =  unlines [unwords $ fmap (showEl r) [1..cs] | r <- [1..rs]]
  where
    showEl r c = fill . show $ xs V.! encode (rs,cs) (r,c)
    fill str = replicate (maxLen - length str) ' ' ++ str
    maxLen = V.maximum $ V.map (length . show) xs


addP :: D3Point -> Matrix D3Coord -> Matrix D3Coord
addP p@(Triple x y z) m@(Matrix r c l v) | space m >= 4 = Matrix r (c+1) (l + 4) (runST $ V.unsafeThaw v >>= addAction)
                                             | otherwise = addP p $ growMat m 1000
  where
    addAction w = set w 1 x >> set w 2 y >> set w 3 z >> set w 4 1 >> V.unsafeFreeze w
    set w n = MV.write w (encode (r,c+1) (n,c+1))
    {-# INLINE set #-}

growMat :: MV.Unbox a => Matrix a -> Int -> Matrix a
growMat m n = m { vector = newV (vector m) }
  where
    newV v = runST $ do
      v' <- V.unsafeThaw v
      v'' <- MV.grow v' n
      V.unsafeFreeze v''

mergeCols :: MV.Unbox a => Matrix a -> Matrix a -> Matrix a
mergeCols b a | rows a  /= rows b = error "The two matrices have a different number of rows"
              | otherwise = runST $ do
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


getD2Point :: Matrix D3Coord -> Int -> D2Point
getD2Point m n = round <$> Pair x y
  where
    x = col `V.unsafeIndex` 0
    y = col `V.unsafeIndex` 1
    col = unsafeGetCol (n+1) m

getD3Point :: Matrix D3Coord -> Int -> D3Point
getD3Point m n = Triple x y z
  where
    x = col `V.unsafeIndex` 0
    y = col `V.unsafeIndex` 1
    z = col `V.unsafeIndex` 2
    col = unsafeGetCol (n+1) m
