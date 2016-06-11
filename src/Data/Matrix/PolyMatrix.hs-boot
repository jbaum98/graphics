module Data.Matrix.PolyMatrix ( PolyMatrix(..) ) where

import Data.D3Point
import Data.Matrix.Base

newtype PolyMatrix = PolyMatrix { runPM :: Matrix D3Coord }