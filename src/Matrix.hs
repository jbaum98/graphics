{-# LANGUAGE TypeOperators, TypeSynonymInstances, FlexibleInstances #-}

module Matrix where

import           Data.Array.Repa hiding (zipWith, size)
import Data.Array.Repa.Repr.Unboxed
import Prelude hiding ()

type Matrix a = Array D DIM2 a

instance (Unbox a, Show a) => Show (Matrix a) where
   show a = dropLast . unlines . fmap unwords $ fmap (fmap show) list
     where list = [ [a `index` (Z :. r :. c) | c <- [0..cols-1] ] | r <- [0..rows-1]]
           Z :. rows :. cols = extent a
           dropLast s = take (length s - 1) s

{-
rowMat :: forall c a. KnownNat c => [a] -> Matrix 1 c a
rowMat = Matrix . listArray ((1, 1), (1, c))
  where
    c = getNat (Proxy :: Proxy c)

colMat :: forall r a. KnownNat r => [a] -> Matrix r 1 a
colMat = Matrix . listArray ((1, 1), (r, 1))
  where
    r = getNat (Proxy :: Proxy r)

listMat :: forall r c a. (KnownNat r, KnownNat c) => [[a]] -> Matrix a
listMat list = Matrix $ array ((1, 1), (rows, cols)) cells
  where
    cells = concat . zipWith zipRow [1 .. rows] $ list
    zipRow rnum = zip [ (rnum, cnum)
                      | cnum <- [1 .. cols] ]
    rows = getNat (Proxy :: Proxy r)
    cols = getNat (Proxy :: Proxy c)

(!) :: forall r c a. (KnownNat r, KnownNat c)
                        => Matrix a
                        -> (Int, Int)
                        -> a
(Matrix a) ! (r, c) = a A.! (r, c)
  where

size :: forall r c a. (KnownNat r, KnownNat c) => Matrix a -> (Int, Int)
size _ = (getNat (Proxy :: Proxy r), getNat (Proxy :: Proxy c))

idMatrix :: (KnownNat s, Num a) => Matrix s s a
idMatrix = genMatrix $ \r c -> if r == c
                                 then 1
                                 else 0

genMatrix :: forall r c a. (KnownNat r, KnownNat c)
                        => (Int -> Int -> a)
                        -> Matrix a
genMatrix f = matrix
                [ ((r, c), f r c)
                | r <- [1 .. rows]
                , c <- [1 .. cols] ]
  where
    rows = getNat (Proxy :: Proxy r)
    cols = getNat (Proxy :: Proxy c)

matMult :: (KnownNat r, KnownNat x, KnownNat c, Num a)
        => Matrix r x a
        -> Matrix x c a
        -> Matrix a
matMult x y = Matrix $
  array ((1, 1), (rows, cols))
    [ ((r, c), dotProd (getRow r x) (getCol c y))
    | r <- [1 .. rows]
    , c <- [1 .. cols] ]
  where
    rows = numRows x
    cols = numCols y

transMatrix :: forall s a. (KnownNat s, Num a) => [a] -> Matrix s s a
transMatrix trans = genMatrix f
  where
    dim = getNat (Proxy :: Proxy s)
    f r c
      | r == c = 1
      | c == dim = trans !! (r - 1)
      | otherwise = 0

scaleMatrix :: (KnownNat s, Num a) => [a] -> Matrix s s a
scaleMatrix scalars = genMatrix f
  where
    f r c
      | r == c = scalars !! c
      | otherwise = 0

degToRad :: Float -> Float
degToRad = (/ 180) . (* pi)

rotXMatrix :: Float -> Matrix 4 4 Float
rotXMatrix degs = listMat $
  [[1, 0, 0, 0], [0, cos theta, -sin theta, 0], [0, sin theta, cos theta, 0], [0, 0, 0, 1]]
  where
    theta = degToRad degs

rotYMatrix :: Float -> Matrix 4 4 Float
rotYMatrix degs = listMat $
  [[cos theta, 0, sin theta, 0], [0, 1, 0, 0], [-sin theta, 0, cos theta, 0], [0, 0, 0, 1]]
  where
    theta = degToRad degs

rotZMatrix :: Float -> Matrix 4 4 Float
rotZMatrix degs = listMat $
  [[cos theta, -sin theta, 0, 0], [sin theta, cos theta, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
  where
    theta = degToRad degs

numRows :: (KnownNat r, KnownNat c) => Matrix a -> Int
numRows = fst . size

numCols :: (KnownNat r, KnownNat c) => Matrix a -> Int
numCols = snd . size

getRow :: (KnownNat r, KnownNat c) => Int -> Matrix a -> [a]
getRow i m@(Matrix a) = [ a A.! (i, c)
                        | c <- [1 .. numCols m] ]

getCol :: (KnownNat r, KnownNat c) => Int -> Matrix a -> [a]
getCol i m@(Matrix a) = [ a A.! (r, i)
                        | r <- [1 .. numRows m] ]

dotProd :: Num a => [a] -> [a] -> a
dotProd l1 l2 = sum $ zipWith (*) l1 l2
 -}
