module Language.MDL (
  parseStr,
  lexMDL,
  parse
  ) where

import Language.MDL.Parser
import Language.MDL.Lexer
import Data.DList

parseStr :: String -> DList Expr
parseStr = runParserM . parse . lexMDL
