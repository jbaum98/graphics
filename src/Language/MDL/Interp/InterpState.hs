module Language.MDL.Interp.InterpState (
  InterpState(..),
  Lighting(..),
  PointLight(..),
  topTransMat,
  initState
  ) where

import GHC.Prim
import Data.Color
import Data.Pair
import Data.Matrix hiding (empty)
import Data.Picture
import Language.MDL.SymTab

data InterpState = InterpState
  { picFunc    :: Picture RealWorld -> IO ()
  , transStack :: ![TransformMatrix]
  , symtab     :: !SymTab
  , lighting   :: !Lighting
  }

data Lighting = Lighting
  { ambient :: !Color
  , lights  :: ![PointLight] }

data PointLight = PointLight
  { color :: !Color
  , loc   :: !(Triple Double) }

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
