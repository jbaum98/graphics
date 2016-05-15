module Language.MDL.Interp.Eval where

import Control.Monad
import Data.Maybe
import System.Process

import D2Point
import Forking
import Language.MDL.Expr
import Language.MDL.Interp.Interp
import Language.MDL.SymTab
import Matrix
import Pbm
import Picture
import Shapes

eval :: Expr -> Interp ()

eval Comment = return ()

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

eval Display = writePicToProcess "display"

eval (Save path) = writePicToProcess $ "convert - " ++ path

-- |
-- = Misc Helpers

scaledMat :: String -> (D3Point -> TransformMatrix) -> D3Point -> Interp ()
scaledMat knob rotMat p =
  getKnob knob >>= multTop . rotMat . (*p) . pure

scaledRot :: String -> (Double -> TransformMatrix) -> Double -> Interp ()
scaledRot knob rotMat degs =
  getKnob knob >>= multTop . rotMat . (*degs)

writePicToProcess :: String -> Interp ()
writePicToProcess cmd = do
  f <- gets picFunc
  s' <- gets maxP
  let pic = f $ blankPic $ fromMaybe (Pair 500 500) s'
  void . liftIO . forkChild $ do
    (Just hin, _, _, ps) <-
      createProcess (shell cmd) { std_in = CreatePipe }
    void $ writePbm pic hin >> waitForProcess ps

roundoff :: D3Point -> D2Point
roundoff (Triple x y _) = round <$> Pair x y
