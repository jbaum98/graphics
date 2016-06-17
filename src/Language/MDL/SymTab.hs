module Language.MDL.SymTab (
  SymTab, Val(..),
  module Data.Map
  ) where

import Data.ByteString.Lazy.Char8
import Data.Map
import Data.Picture.Drawing.Lighting

import Data.Matrix

-- | A symbol table is used to store the values of variables
type SymTab = Map ByteString Val

-- | There are multiple types of variables stored in the symbol table
data Val = DoubleVal Double          -- ^ Numeric knob values
         | MatrixVal (Matrix Double) -- ^ Transformation matrixes for storing
                                     -- coordinate systems
         | ConstsVal LightingConsts  -- ^ Lighting constants for storing
                                     -- materials
