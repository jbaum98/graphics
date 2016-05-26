{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveGeneric, DeriveAnyClass #-}

module Data.Pair.Triple (Triple(..), uncurryTriple) where

import Control.Monad ( liftM )
import Control.Applicative
import GHC.Generics

import Control.DeepSeq

import Data.Vector.Unboxed.Base
import GHC.Arr

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

-- |Represents three objects of the same type
data Triple a = Triple !a !a !a
              deriving (Show, Eq, Ord, Generic, NFData)

-- |Converts a function taking three arguments of the same type
-- to a function taking single 'Triple'
uncurryTriple :: (a -> a -> a -> b) -> Triple a -> b
uncurryTriple f (Triple a1 a2 a3) = f a1 a2 a3

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

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c
{-# INLINE uncurry3 #-}

newtype instance MVector s (Triple a) = MV_Triple (MVector s (a,a,a))
newtype instance Vector    (Triple a) = V_Triple  (Vector    (a,a,a))

instance Unbox a => Unbox (Triple a)

instance Unbox a => M.MVector MVector (Triple a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Triple v) = M.basicLength v
  basicUnsafeSlice i n (MV_Triple v) = MV_Triple $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Triple v1) (MV_Triple v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Triple `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Triple v) = M.basicInitialize v
  basicUnsafeReplicate n (Triple x y z) = MV_Triple `liftM` M.basicUnsafeReplicate n (x,y, z)
  basicUnsafeRead (MV_Triple v) i = uncurry3 Triple `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Triple v) i (Triple x y z) = M.basicUnsafeWrite v i (x,y,z)
  basicClear (MV_Triple v) = M.basicClear v
  basicSet (MV_Triple v) (Triple x y z) = M.basicSet v (x,y,z)
  basicUnsafeCopy (MV_Triple v1) (MV_Triple v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Triple v1) (MV_Triple v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Triple v) n = MV_Triple `liftM` M.basicUnsafeGrow v n

instance Unbox a => G.Vector Vector (Triple a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Triple v) = V_Triple `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Triple v) = MV_Triple `liftM` G.basicUnsafeThaw v
  basicLength (V_Triple v) = G.basicLength v
  basicUnsafeSlice i n (V_Triple v) = V_Triple $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Triple v) i
                = uncurry3 Triple `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Triple mv) (V_Triple v)
                = G.basicUnsafeCopy mv v
  elemseq _ (Triple x y z) w = G.elemseq (undefined :: Vector a) x
                       $ G.elemseq (undefined :: Vector a) y
                       $ G.elemseq (undefined :: Vector a) z w

instance Ix a => Ix (Triple a)  where
    {-# SPECIALISE instance Ix (Triple Int) #-}

    range (Triple l1 l2 l3, Triple u1 u2 u3) =
        [Triple i1 i2 i3 | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3)]

    unsafeIndex (Triple l1 l2 l3, Triple u1 u2 u3) (Triple i1 i2 i3) =
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
      unsafeIndex (l1,u1) i1)

    inRange (Triple l1 l2 l3, Triple u1 u2 u3) (Triple i1 i2 i3) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3

{-# RULES
"safeIndex/Triple I" safeIndex = lessSafeIndex :: (Triple Int, Triple Int) -> Int -> Triple Int -> Int
  #-}
