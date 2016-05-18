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

import qualified Data.ByteString.Lazy as ByteString

parseStr :: ByteString.ByteString -> DList Expr
parseStr = parse . lexMDL
