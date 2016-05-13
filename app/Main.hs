module Main where

import System.Environment

import D2Point
import Parser
import Forking

main :: IO ()
main = do
  args <- getArgs
  respond args
  waitForChildren

respond :: [String] -> IO ()
respond [] = putStrLn "You didn't supply any args\nUsage: ./main <script file>"
respond (path:_) = execScript path (Pair 500 500)

execScript :: FilePath -> D2Point -> IO ()
execScript path maxPoint = do
  parse <- readScript path
  case parse of
    Left err -> putStrLn $ "Error on parsing: " ++ err
    Right cmds -> execute maxPoint cmds
