module Language.MDL (
  parseStr,
  lexMDL,
  parse,
  execute
  ) where

import Language.MDL.Parser
import Language.MDL.Lexer
import Language.MDL.Interp
import Data.DList

parseStr :: String -> DList Expr
parseStr = parse . lexMDL
