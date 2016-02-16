module Pair where

import Control.Applicative

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure f = Pair f f
  Pair f1 f2 <*> Pair a b = Pair (f1 a) (f2 b)

data Triple a = Triple a a a deriving (Show, Eq)

instance Functor Triple where
  fmap f (Triple a b c) = Triple (f a) (f b) (f c)

instance Applicative Triple where
  pure f = Triple f f f
  Triple f1 f2 f3 <*> Triple a b c = Triple (f1 a) (f2 b) (f3 c)

uncurryPair :: (a -> a -> b) -> Pair a -> b
uncurryPair f (Pair a1 a2) = f a1 a2

uncurryTriple :: (a -> a -> a -> b) -> Triple a -> b
uncurryTriple f (Triple a1 a2 a3) = f a1 a2 a3
