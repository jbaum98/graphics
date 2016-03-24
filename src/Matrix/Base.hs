module Matrix.Base (
  Matrix, U, D,
  fromListUnboxed,
  prettyPrint,
  ) where

import           Data.Array.Repa

type Matrix r a = Array r DIM2 a

prettyPrint :: (Show e, Source r e) => Array r DIM2 e -> IO ()
prettyPrint a = putStrLn . dropLast . unlines . fmap unwords $ fmap (fmap show) list
  where
    list = [ [ a `index` (Z :. r :. c)
             | c <- [0 .. cols - 1] ]
           | r <- [0 .. rows - 1] ]
    Z :. rows :. cols = extent a
    dropLast s = take (length s - 1) s
