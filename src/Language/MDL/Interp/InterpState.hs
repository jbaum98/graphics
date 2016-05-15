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
  , nframes        :: !Int
  , basename       :: !(Maybe FilePath)
  }

topTransMat :: InterpState -> TransformMatrix
topTransMat ps = case transStack ps of
                       (top:_) -> top
                       []      -> idMatrix

initState :: InterpState
initState    = InterpState
  { picFunc    = id
  , transStack = []
  , maxP       = Nothing
  , symtab     = empty
  , nframes    = 1
  , basename   = Nothing }