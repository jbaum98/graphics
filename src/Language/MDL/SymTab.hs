module Language.MDL.SymTab (
  SymTab, Val(..),
  module Data.Map
  ) where

import Data.Pair
import Data.Map
import Data.ByteString.Lazy.Char8

import Data.Matrix

type SymTab = Map ByteString Val

data Val = DoubleVal Double
         | MatrixVal (Matrix Double)
         | ConstsVal (Triple (Triple Double))
