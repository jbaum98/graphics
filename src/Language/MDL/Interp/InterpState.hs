module Language.MDL.Interp.InterpState (
  InterpState(..),
  topTransMat,
  initState
  ) where

import Language.MDL.SymTab
import Matrix hiding (empty)
import Picture

data InterpState = InterpState
  { picFunc        :: !(Picture -> Picture)
  , transStack     :: ![TransformMatrix]
  , maxP           :: !(Maybe D2Point)
  , symtab         :: !SymTab
  }

topTransMat :: InterpState -> TransformMatrix
topTransMat ps = case transStack ps of
                       (top:_) -> top
                       []      -> idMatrix

initState :: InterpState
initState = InterpState id [] Nothing empty
