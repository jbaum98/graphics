module Language.MDL.SymTab (
  SymTab, Val(..),
  module Data.Map
  ) where

import Data.Map

import Matrix

type SymTab = Map String Val

data Val = DoubleVal Double
         | MatrixVal (Matrix Double)
         -- | ConstantsVal Constants
         -- | LightVal Light
