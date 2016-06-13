module Data.Matrix.PolyMatrix ( PolyMatrix(..) ) where

import Data.Matrix.Base

newtype PolyMatrix = PolyMatrix { runPM :: Matrix Double }
