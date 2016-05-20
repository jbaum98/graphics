{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import System.Environment

import qualified Data.ByteString.Lazy as B

import Language.MDL

main :: IO ()
main = do
  args <- getArgs
  respond args

respond :: [String] -> IO ()
respond [] = putStrLn "You didn't supply any args\nUsage: ./main <script file>"
respond (path:rest) | "-v" `elem` rest = execScript path True
respond (path:_) = execScript path False

execScript :: FilePath -> Bool -> IO ()
execScript path verbose = do
 s <- B.readFile path
 let !ast = parseStr s
 when verbose $ mapM_ print ast
 execute ast
