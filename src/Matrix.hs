module Matrix (
  Matrix, prettyPrint,
  -- * Transformation
  idMatrix, matMult,
  transMatrix, scaleMatrix, rotXMatrix, rotYMatrix, rotZMatrix,
  -- * EdgeMatrix
  module Matrix.EdgeMatrix
  ) where

import Matrix.Base
import Matrix.Mult
import Matrix.Transform
import Matrix.EdgeMatrix
