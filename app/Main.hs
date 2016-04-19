module Main where

import Pair
import System.Environment
import GenerateFile

main :: IO ()
main = do args <- getArgs
          respond args

respond :: [String] -> IO ()
respond [] = putStrLn "You didn't supply any args\nUsage: ./main <script file>"
respond (path:_) = generateFile path (Pair 1000 1000)
