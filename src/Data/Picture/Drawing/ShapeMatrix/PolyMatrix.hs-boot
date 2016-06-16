module Data.Picture.Drawing.ShapeMatrix.PolyMatrix ( PolyMatrix(..) ) where

import Data.Matrix

newtype PolyMatrix = PolyMatrix { runPM :: Matrix Double }
