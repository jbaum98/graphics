module Data.Matrix (
  Matrix,
  rows,
  cols,
  size,
  prettyMatrix,
  -- * Construction
  empty,
  emptyWith,
  fromList,
  fromLists,
  fromFunction,
  -- ** Adding Points
  addP,
  growMat,
  mergeCols,
  -- * Accessing
  (!),
  getCol,
  getRow,
  unsafeIndex,
  unsafeGetCol,
  unsafeGetRow,
  -- ** Extracting Points
  get2DPoint,
  get3DPoint,
  module X
  ) where

import Data.Matrix.Transform as X
import Data.Matrix.Mult as X
import Data.Matrix.Base
import Data.Matrix.Points
