module Main where

import System.Environment

import D2Point
import Forking
import Language.MDL
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  respond args
  waitForChildren

respond :: [String] -> IO ()
respond [] = putStrLn "You didn't supply any args\nUsage: ./main <script file>"
respond (path:rest) | "-v" `elem` rest = execScript path (Pair 500 500) True
respond (path:_) = execScript path (Pair 500 500) False

execScript :: FilePath -> D2Point -> Bool -> IO ()
execScript path maxPoint verbose = do
  s <- readFile path
  let ast = parseStr s
  execute ast
  when verbose $
    mapM_ print ast
