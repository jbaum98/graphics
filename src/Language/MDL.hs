module Language.MDL (
  parseStr,
  lexMDL,
  parseMDL,
  execute
  ) where

import Language.MDL.Parser
import Language.MDL.Lexer
import Language.MDL.Interp
import Data.DList

import qualified Data.ByteString.Lazy as ByteString

-- | Parses a script into a 'DList' of 'Expr's
parseStr :: ByteString.ByteString -> DList Expr
parseStr = parseMDL . lexMDL
