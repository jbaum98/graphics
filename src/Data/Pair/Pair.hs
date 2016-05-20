{-# LANGUAGE DeriveGeneric #-}

module Data.Pair.Pair (Pair(..), uncurryPair) where

import Control.Applicative
import GHC.Generics

import Control.DeepSeq

-- |Represents two objects of the same type
data Pair a = Pair !a !a
            deriving (Show, Eq, Generic)

instance NFData a => NFData (Pair a)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure f = Pair f f
  Pair f1 f2 <*> Pair a b = Pair (f1 a) (f2 b)

instance Foldable Pair where
  foldMap m (Pair a1 a2) = m a1 `mappend` m a2

instance Traversable Pair where
  traverse f (Pair a1 a2) = Pair <$> f a1 <*> f a2

instance Num a => Num (Pair a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = liftA abs
  negate = liftA negate
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Pair a) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = pure . fromRational

-- |Converts a function taking two arguments of the same type
-- to a function taking single 'Pair'
uncurryPair :: (a -> a -> b) -> Pair a -> b
uncurryPair f (Pair a1 a2) = f a1 a2
