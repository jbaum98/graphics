module Language.MDL.SymTab (
  SymTab, Val(..),
  module Data.Map
  ) where

import Data.Map
import Data.ByteString.Lazy.Char8

import Matrix

type SymTab = Map ByteString Val

data Val = DoubleVal Double
         | MatrixVal (Matrix Double)
         -- | ConstantsVal Constants
         -- | LightVal Light
