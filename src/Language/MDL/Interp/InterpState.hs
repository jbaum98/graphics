{-|
Module      : Language.MDL.Interp.InterpState
Description : Stores state for the interpreter

The 'InterpState' data structure stores all the state of the interpreter as it
executes each 'Expr'
-}

module Language.MDL.Interp.InterpState (
  InterpState(..),
  Lighting(..),
  PointLight(..),
  ShadingType(..),
  topTransMat,
  initState
  ) where

import GHC.Prim
import Data.Matrix hiding (empty)
import Data.Picture
import Language.MDL.SymTab

data InterpState = InterpState
  {
    -- | A function that modifies a blank picture to create the desired picture
    picFunc    :: Picture RealWorld -> IO ()
  , transStack :: ![TransformMatrix] -- ^ The transformation stack
  , symtab     :: !SymTab            -- ^ The symbol table
  , lighting   :: !Lighting          -- ^ The lighting information
  , shading    :: !ShadingType       -- ^ The shading type
  }

-- | Get the top of the transformation stack from an 'InterpState'
topTransMat :: InterpState -> TransformMatrix
topTransMat ps = case transStack ps of
                       (top:_) -> top
                       []      -> idMatrix

-- | The initial lighting information
initLighting :: Lighting
initLighting = Lighting (pure 0) []

-- | The initial state of an interpreter
initState :: InterpState
initState = InterpState
  { picFunc    = const $ return ()
  , transStack = []
  , symtab     = empty
  , lighting   = initLighting
  , shading    = Flat }
