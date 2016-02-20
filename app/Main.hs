module Main where

import System.Environment
import MakeFile

main :: IO ()
main = do args <- getArgs
          respond args

respond :: [String] -> IO ()
respond [] = putStrLn "You didn't supply any args\nUsage: ./main <output file>"
respond (path:_) = makeFile path
