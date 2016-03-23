module Utils (compose, applyAll) where

import Data.Foldable
import Prelude hiding (foldl, foldr)

compose :: [a -> a] -> a -> a
compose = foldr (.) id

applyAll :: Functor f => f (a -> b) -> a -> f b
applyAll fs a = fmap ($a) fs
