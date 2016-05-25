module Language.MDL.Interp.InterpState (
  InterpState(..),
  topTransMat,
  initState
  ) where

import Data.D2Point
import Data.Matrix hiding (empty)
import Data.Picture
import Language.MDL.SymTab

data InterpState = InterpState
  { picFunc        :: Picture -> IO ()
  , transStack     :: ![TransformMatrix]
  , maxP           :: !(Maybe D2Point)
  , symtab         :: !SymTab
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
  , symtab     = empty }
