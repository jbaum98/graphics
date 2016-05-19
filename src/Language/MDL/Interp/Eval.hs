{-# LANGUAGE ScopedTypeVariables #-}

module Language.MDL.Interp.Eval where

import Control.Monad
import Data.Maybe
import System.IO
import System.Posix.IO
import System.Posix.Process
import qualified Control.Exception(try, IOException)

import D2Point
import Language.MDL.Expr
import Language.MDL.Interp.Interp
import Language.MDL.SymTab
import Matrix
import Pbm
import Picture
import Shapes
import Data.ByteString.Lazy.Char8

eval :: Expr -> Interp ()

eval Comment = return ()
eval Basename {} = return ()
eval Frames {} = return ()
eval Vary {} = return ()

eval (Set knob val) = modSymTab $ insert knob (DoubleVal val)

eval (Line _ p1 cs1 p2 cs2) = do
  tm1 <- getTM cs1
  tm2 <- getTM cs2
  let trans tm = roundoff . transform tm
  addF $ drawLine (trans tm1 p1) (trans tm2 p2)

eval (Box _ topLeft dims cs) = drawInCS cs $ box topLeft dims

eval (Sphere _ center r cs) = drawInCS cs $ sphere center r 10

eval (Torus _ center r1 r2 cs) = drawInCS cs $ torus center r1 r2 10

eval (Move trans Nothing) = multTop $ transMatrix trans
eval (Move trans (Just knob)) = scaledMat knob transMatrix trans

eval (Scale scalars Nothing) = multTop $ scaleMatrix scalars
eval (Scale scalars (Just knob)) = scaledMat knob scaleMatrix scalars

eval (Rotate X degs Nothing) = multTop $ rotXMatrix degs
eval (Rotate Y degs Nothing) = multTop $ rotYMatrix degs
eval (Rotate Z degs Nothing) = multTop $ rotZMatrix degs
eval (Rotate X degs (Just knob)) = scaledRot knob rotYMatrix degs
eval (Rotate Y degs (Just knob)) = scaledRot knob rotYMatrix degs
eval (Rotate Z degs (Just knob)) = scaledRot knob rotZMatrix degs

eval Push = modTransStack pushF
  where pushF (oldTM:rest) = oldTM : oldTM : rest
        pushF [] = [idMatrix]

eval Pop = modTransStack popF
  where popF (_:rest) = rest
        popF [] = []

eval Display = writePicToProcess "display" []

eval (Save path) = writePicToProcess "convert" ["-", unpack path]

-- |
-- = Misc Helpers

scaledMat :: ByteString -> (D3Point -> TransformMatrix) -> D3Point -> Interp ()
scaledMat knob rotMat p =
  getKnob knob >>= multTop . rotMat . (*p) . pure

scaledRot :: ByteString -> (Double -> TransformMatrix) -> Double -> Interp ()
scaledRot knob rotMat degs =
  getKnob knob >>= multTop . rotMat . (*degs)

writePicToProcess :: String -> [String] -> Interp ()
writePicToProcess cmd args = do
  f  <- gets picFunc
  s' <- gets maxP
  let pic = f $ blankPic $ fromMaybe (Pair 500 500) s'
  liftIO $ pOpen cmd args $ writePbm pic

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

roundoff :: D3Point -> D2Point
roundoff (Triple x y _) = round <$> Pair x y
