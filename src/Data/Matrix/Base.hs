{-|
Module      : Matrix.Base
Description : Defines a basic Matrix type

Defines a basic Matrix type using unboxed 'Vector's.
-}

module Data.Matrix.Base (
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
    decode
    ) where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.Loop (numLoop)

data Matrix a = Matrix
  { rows      :: {-# UNPACK #-} !Int -- ^The number of rows.
  , cols      :: {-# UNPACK #-} !Int -- ^The number of columns.

  -- | The last index in use in the underlying 'Vector'.
  , lastIndex :: {-# UNPACK #-} !Int

   -- | The underlying 'Vector' used to store the elements
  , vector    ::                 Vector a
  }

instance (Show a, MV.Unbox a) => Show (Matrix a) where
  show (Matrix r c l v) =
    "fromList (" ++ show r ++ "," ++ show c ++ ") " ++ show l ++ " " ++ show v

-- | The number of rows and columns of the 'Matrix' in a tuple @(rows,cols)@.
size :: Matrix a -> (Int,Int)
size (Matrix r c _ _) = (r,c)
{-# INLINE size #-}

-- | The amount of space remaining in the underlying 'Vector'.
space :: MV.Unbox a => Matrix a -> Int
space m = V.length (vector m) - lastIndex m - 1
{-# INLINE space #-}

-- | The length of the underlying 'Vector'
len :: MV.Unbox a => Matrix a -> Int
len = V.length . vector
{-# INLINE len #-}

-- | Convert a tuple @(row,col)@ into the corresponding index of the underlying
-- 'Vector'.
encode :: (Int,Int) -- ^The size of the 'Matrix'
       -> (Int,Int) -- ^The tuple @(row,col)@ to be converted
       -> Int
encode (rs,_) (i,j) = (j-1)*rs + i - 1
{-# INLINE encode #-}

-- | Convert an index of the underlying 'Vector' into the corresponding tuple
-- @(row,col)@.
decode :: (Int,Int) -- ^The size of the 'Matrix'
       -> Int
       -> (Int,Int)
decode (rs,_) k = (r+1,q+1)
 where
  (q,r) = quotRem k rs
{-# INLINE decode #-}

-- | Compute the minimum length of a list necessary to store the elements of a
-- 'Matrix'.
listLen :: (Int,Int) -- ^The size of the 'Matrix'
        -> Int
listLen (r,c) = r * c
{-# INLINE listLen #-}

-- | Determine if a location is within the bounds of a 'Matrix'.
inRange :: (Int,Int) -- ^The size of the 'Matrix'
        -> (Int,Int) -> Bool
(r,c) `inRange` (rs, cs) = r <= rs &&
                           c <= cs &&
                           r > 0  &&
                           c > 0
{-# INLINE inRange #-}

-- | Construct a 'Matrix' from a list.
fromList :: MV.Unbox a
         => (Int,Int) -- ^The size of the 'Matrix'
         -> [a] -> Matrix a
fromList s xs = uncurry Matrix s (listLen s - 1) (V.fromListN (listLen s) xs)
{-# INLINE fromList #-}

-- | Construct a 'Matrix' from a function.
fromFunction :: MV.Unbox a
             => (Int,Int)       -- ^The size of the 'Matrix'
             -> ((Int,Int) -> a) -- ^The generating function
             -> Matrix a
fromFunction s@(rs,cs) f = Matrix rs cs (listLen s - 1) $ V.create $ do
  v <- MV.new $ rs * cs
  let en = encode s
  numLoop 1 rs $
    \i -> numLoop 1 cs $
    \j -> MV.unsafeWrite v (en (i,j)) (f (i,j))
  return v
{-# INLINE fromFunction #-}

-- | Construct a 'Matrix' from a list of rows.
fromLists :: MV.Unbox a => [[a]] -> Matrix a
fromLists xss = fromFunction (r,c) $ \(i,j) -> (xss !! (i - 1)) !! (j - 1)
  where
    r = length xss
    c = length $ head xss
{-# INLINE fromLists #-}

unsafeIndex, (!) :: MV.Unbox a => Matrix a -> (Int,Int) -> a

-- | Access the element at a location in a 'Matrix'. No bounds checks.
(Matrix r c _ xs) `unsafeIndex` i = xs `V.unsafeIndex` encode (r,c) i
{-# INLINE unsafeIndex #-}

-- | Access the element at a location in a 'Matrix'.
(!) m i
  |  i `inRange` size m = m `unsafeIndex` i
  |  otherwise = error $
    "The provided index (" ++ show i ++
     ") is out of bounds for a matrix of size " ++ showSize (size m)
     where
       showSize (r,c) = show r ++ "x" ++ show c
{-# INLINE (!) #-}

unsafeGetCol, getCol :: MV.Unbox a => Int -> Matrix a -> Vector a

-- | Get an entire column of a 'Matrix'. This is faster than getting a row
-- because of the way elements are arranged internally. No bounds checks.
unsafeGetCol j (Matrix r _ _ v) = V.unsafeSlice ((j-1) * r) r v
{-# INLINE unsafeGetCol #-}

-- | Get an entire column of a 'Matrix'. This is faster than getting a row
-- because of the way elements are arranged internally.
getCol j m
  |  j <= cols m && j > 0 = unsafeGetCol j m
  |  otherwise = error $
      "getCol: column index " ++ show j ++ "out of bounds for matrix with " ++
      show (cols m) ++ " columns"
{-# INLINE getCol #-}

unsafeGetRow, getRow :: MV.Unbox a => Int -> Matrix a -> Vector a

-- | Get an entire row of a 'Matrix'. No bounds checks.
unsafeGetRow i m =
  V.generate (cols m) $ \j -> vector m `V.unsafeIndex` encode (size m) (i,j+1)
{-# INLINE unsafeGetRow #-}

-- | Get an entire row of a 'Matrix'.
getRow i m
  |  i <= rows m = unsafeGetRow i m
  |  otherwise = error $
    "getRow: row index " ++ show i ++ "out of bounds for matrix with " ++
    show (rows m) ++ " rows"
{-# INLINE getRow #-}

-- | Prints a 'Matrix' in rows and columns. Doesn't align elements if they are
-- too long.
prettyMatrix :: (Show a, MV.Unbox a) => Matrix a -> String
prettyMatrix (Matrix rs cs _ xs) =
  unlines [unwords $ fmap (showEl r) [1..cs] |  r <- [1..rs]]
  where
    showEl r c = fill . show $ xs V.! encode (rs,cs) (r,c)
    fill str = replicate (maxLen - length str) ' ' ++ str
    maxLen = V.maximum $ V.map (length . show) xs
