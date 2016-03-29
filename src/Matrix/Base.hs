{-|
Module      : Matrix.Base
Description : Defines a basic Matrix type

Defines a basic Matrix type using REPA arrays.
-}

module Matrix.Base (
    Matrix,
    U,
    D,
    fromListUnboxed,
    prettyPrint,
    ) where

import           Data.Array.Repa

-- |A 'Matrix' is a 2D Repa 'Array' with representation @r@
--  and holding elements of type @a@
type Matrix r a = Array r DIM2 a

-- |Prints a 'Matrix' in rows and columns.
--  Doesn't align elements if they are too long.
prettyPrint :: (Show e, Source r e) => Matrix r e -> String
prettyPrint a = dropLast . unlines . fmap unwords $ fmap (fmap show) list
  where
    list = [ [ a `index` (Z :. r :. c)
             | c <- [0 .. cols - 1] ]
           | r <- [0 .. rows - 1] ]
    Z :. rows :. cols = extent a
    dropLast s = take (length s - 1) s
