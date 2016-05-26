module Language.MDL.Interp.InterpState (
  InterpState(..),
  topTransMat,
  initState
  ) where

import GHC.Prim
import Data.Matrix hiding (empty)
import Data.Picture
import Language.MDL.SymTab

data InterpState = InterpState
  { picFunc    :: Picture RealWorld -> IO ()
  , transStack :: ![TransformMatrix]
  , symtab     :: !SymTab
  }

topTransMat :: InterpState -> TransformMatrix
topTransMat ps = case transStack ps of
                       (top:_) -> top
                       []      -> idMatrix

initState :: InterpState
initState = InterpState
  { picFunc    = const $ return ()
  , transStack = []
  , symtab     = empty }
