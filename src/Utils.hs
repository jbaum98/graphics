module Utils (compose, applyAll) where

compose :: [a -> a] -> a -> a
compose = foldl (.) id

applyAll :: Functor f => f (a -> b) -> a -> f b
applyAll fs a = fmap ($a) fs
