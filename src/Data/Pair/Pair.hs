{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveGeneric, DeriveAnyClass #-}

module Data.Pair.Pair (Pair(..), uncurryPair) where

import Control.Monad ( liftM )
import Control.Applicative
import GHC.Generics

import Control.DeepSeq

import Data.Vector.Unboxed.Base
import GHC.Arr

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

-- |Represents two objects of the same type
data Pair a = Pair !a !a
            deriving (Show, Eq, Ord, Generic, NFData)

-- |Converts a function taking two arguments of the same type
-- to a function taking single 'Pair'
uncurryPair :: (a -> a -> b) -> Pair a -> b
uncurryPair f (Pair a1 a2) = f a1 a2

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

newtype instance MVector s (Pair a) = MV_Pair (MVector s (a,a))
newtype instance Vector    (Pair a) = V_Pair  (Vector    (a,a))

instance Unbox a => Unbox (Pair a)

instance Unbox a => M.MVector MVector (Pair a) where
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
  basicLength (MV_Pair v) = M.basicLength v
  basicUnsafeSlice i n (MV_Pair v) = MV_Pair $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Pair v1) (MV_Pair v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Pair `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Pair v) = M.basicInitialize v
  basicUnsafeReplicate n (Pair x y) = MV_Pair `liftM` M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Pair v) i = uncurry Pair `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Pair v) i (Pair x y) = M.basicUnsafeWrite v i (x,y)
  basicClear (MV_Pair v) = M.basicClear v
  basicSet (MV_Pair v) (Pair x y) = M.basicSet v (x,y)
  basicUnsafeCopy (MV_Pair v1) (MV_Pair v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Pair v1) (MV_Pair v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Pair v) n = MV_Pair `liftM` M.basicUnsafeGrow v n

instance Unbox a => G.Vector Vector (Pair a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Pair v) = V_Pair `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Pair v) = MV_Pair `liftM` G.basicUnsafeThaw v
  basicLength (V_Pair v) = G.basicLength v
  basicUnsafeSlice i n (V_Pair v) = V_Pair $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Pair v) i
                = uncurry Pair `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Pair mv) (V_Pair v)
                = G.basicUnsafeCopy mv v
  elemseq _ (Pair x y) z = G.elemseq (undefined :: Vector a) x
                       $ G.elemseq (undefined :: Vector a) y z

instance Ix a => Ix (Pair a) where
  {-# SPECIALISE instance Ix (Pair Int) #-}

  {-# INLINE range #-}
  range (Pair l1 l2, Pair u1 u2) =
    [ Pair i1 i2 | i1 <- range (l1,u1), i2 <- range (l2,u2) ]

  {-# INLINE unsafeIndex #-}
  unsafeIndex (Pair l1 l2,Pair u1 u2) (Pair i1 i2) =
    unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2

  {-# INLINE inRange #-}
  inRange (Pair l1 l2,Pair u1 u2) (Pair i1 i2) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2

{-# RULES
"safeIndex/Pair I"   safeIndex = lessSafeIndex :: (Pair Int, Pair Int) -> Int -> Pair Int -> Int
  #-}
