{-# LANGUAGE DeriveGeneric #-}

module Data.Pair.Triple (Triple(..), uncurryTriple) where

import Control.Applicative

import Control.DeepSeq

-- |Represents three objects of the same type
data Triple a = Triple !a !a !a
              deriving (Show, Eq, Ord)

instance NFData a => NFData (Triple a) where
  rnf (Triple a b c) = rnf a `seq` rnf b `seq` rnf c

instance Functor Triple where
  fmap f (Triple a b c) = Triple (f a) (f b) (f c)

instance Applicative Triple where
  pure f = Triple f f f
  Triple f1 f2 f3 <*> Triple a b c = Triple (f1 a) (f2 b) (f3 c)

instance Foldable Triple where
  foldMap m (Triple a1 a2 a3) = m a1 `mappend` m a2 `mappend` m a3

instance Traversable Triple where
  traverse f (Triple a1 a2 a3) = Triple <$> f a1 <*> f a2 <*> f a3

instance Num a => Num (Triple a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = liftA abs
  negate = liftA negate
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Triple a) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = pure . fromRational

-- |Converts a function taking three arguments of the same type
-- to a function taking single 'Triple'
uncurryTriple :: (a -> a -> a -> b) -> Triple a -> b
uncurryTriple f (Triple a1 a2 a3) = f a1 a2 a3
