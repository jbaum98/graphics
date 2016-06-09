module Language.MDL.Interp.InterpState (
  InterpState(..),
  Lighting(..),
  PointLight(..),
  topTransMat,
  initState
  ) where

import GHC.Prim
import Data.Matrix hiding (empty)
import Data.Picture
import Language.MDL.SymTab
import Data.Picture.Drawing.Lighting

data InterpState = InterpState
  { picFunc    :: Picture RealWorld -> IO ()
  , transStack :: ![TransformMatrix]
  , symtab     :: !SymTab
  , lighting   :: !Lighting
  }

topTransMat :: InterpState -> TransformMatrix
topTransMat ps = case transStack ps of
                       (top:_) -> top
                       []      -> idMatrix

initLighting :: Lighting
initLighting = Lighting (pure 0) []

initState :: InterpState
initState = InterpState
  { picFunc    = const $ return ()
  , transStack = []
  , symtab     = empty
  , lighting   = initLighting }
