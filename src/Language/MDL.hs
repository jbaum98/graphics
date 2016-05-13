module Language.MDL (
  parseStr,
  lexMDL,
  parse
  ) where

import Language.MDL.Parser
import Language.MDL.Lexer

parseStr :: String -> [Syntax]
parseStr = parse . lexMDL
