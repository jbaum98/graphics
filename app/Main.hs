{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Main where

import System.IO
import System.Environment

import D2Point
import Language.MDL
import Control.Monad
import Picture
import Pbm

import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  args <- getArgs
  respond args

respond :: [String] -> IO ()
respond [] = putStrLn "You didn't supply any args\nUsage: ./main <script file>"
respond (path:rest) | "-v" `elem` rest = execScript path (Pair 500 500) True
respond (path:_) = execScript path (Pair 500 500) False

execScript :: FilePath -> D2Point -> Bool -> IO ()
execScript path maxPoint verbose = do
 s <- B.readFile path
 let !ast = parseStr s
 when verbose $ mapM_ print ast
 execute ast
