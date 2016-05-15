module Language.MDL.Interp (
  execute
  ) where

import Language.MDL.Expr
import Language.MDL.Interp.Eval
import Language.MDL.Interp.Interp

execute :: Foldable f => f Expr -> IO ()
execute = flip evalInterp initState . mapM_ eval
