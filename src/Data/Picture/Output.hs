{-# LANGUAGE ScopedTypeVariables #-}

module Data.Picture.Output (
  writePbm,
  savePbm,
  writePicToProcess,
  savePic,
  displayPic
  ) where

import Control.Monad
import System.IO
import Control.Monad.Primitive

import System.Posix.IO
import System.Posix.Process
import qualified Control.Exception(try, IOException)

import Data.Picture.Output.Pbm as X
import Data.Picture.Picture

savePic :: FilePath -> Picture RealWorld -> IO ()
savePic name = writePicToProcess "convert" ["-", name]

displayPic :: Picture RealWorld -> IO ()
displayPic = writePicToProcess "display" []

writePicToProcess :: FilePath -> [String] -> Picture RealWorld -> IO ()
writePicToProcess cmd args pic =
  pOpen cmd args $ writePbm pic

pOpen :: FilePath -> [String] -> (Handle -> IO a) -> IO a
pOpen fp args func = do
  pipepair <- createPipe
  pid <- do
    p <- Control.Exception.try (forkProcess $ childstuff pipepair)
    case p of
      Right x -> return x
      Left (e :: Control.Exception.IOException) -> fail ("Error in fork: " ++ show e)
  retval <- callfunc pipepair
  let rv = seq retval retval
  void $ getProcessStatus True False pid
  return rv
  where
    callfunc pipepair = do
      closeFd (fst pipepair)
      h <- fdToHandle (snd pipepair)
      x <- func h
      hClose h
      return $! x

    childstuff pipepair = do
              void $ dupTo (fst pipepair) stdInput
              closeFd $ fst pipepair
              closeFd $ snd pipepair
              executeFile fp True args Nothing
