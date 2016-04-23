{-# LANGUAGE TupleSections #-}

module Parser.Interp (
  execute,
  runEval,
  eval
  ) where

import Picture
import Pbm
import Matrix hiding ((++))
import qualified Matrix as M ((++))
import Parser.Parser
import Control.Monad.State
import System.Process
import Shapes

type ParseState = (EdgeMatrix, TransformMatrix, D2Point)
type Interp = StateT ParseState IO ()

execute :: D2Point -> [Command] -> IO ()
execute s = runEval s . mapM_ eval

runEval :: D2Point -> Interp -> IO ()
runEval = flip evalStateT . initState

initState :: D2Point -> ParseState
initState = (empty, idMatrix, )

-- eval :: Command -> ParseState -> ((IO (), ParseState))
eval :: Command -> Interp
eval (Line x0 y0 z0 x1 y1 z1) = addEdges $ edge p0 p1
  where
    p0 = Triple x0 y0 z0
    p1 = Triple x1 y1 z1

eval (Circle cx cy r) = addEdges $ circle center r
  where center = Triple cx cy 0

eval (Hermite x0 y0 x1 y1 x2 y2 x3 y3) = addEdges $ hermite p0 r0 p1 r1
  where
   p0   = Triple x0 y0 0
   p1   = Triple x2 y2 0
   ctl0 = Triple x1 y1 0
   ctl1 = Triple x3 y3 0
   r0   = ctl0 - p0
   r1   = ctl1 - p1

eval (Bezier x1 y1 x2 y2 x3 y3 x4 y4) = addEdges $ bezier p1 p2 p3 p4
  where
    p1  = Triple x1 y1 0
    p2  = Triple x2 y2 0
    p3  = Triple x3 y3 0
    p4  = Triple x4 y4 0

eval Identity = do
  (em, _, s) <- get
  put (em, idMatrix, s)

eval (Scale sx sy sz) = addTrans . scaleMatrix $ Triple sx sy sz

eval (Translate tx ty tz) = addTrans . transMatrix $ Triple tx ty tz

eval (RotateX degs) = addTrans $ rotXMatrix degs

eval (RotateY degs) = addTrans $ rotYMatrix degs

eval (RotateZ degs) = addTrans $ rotZMatrix degs

eval Apply = do
  (em, tm, s) <- get
  put (tm `matMult` em, tm, s)

eval Display = writePicToProcess "display"

eval (Save path) = writePicToProcess $ "convert - " ++ path

writePicToProcess :: String -> Interp
writePicToProcess cmd = do
  (em, tm, s) <- get
  let pic = drawLines em (blankPic s)
  void . liftIO $ do
    (Just hin, _, _, ps) <-
      createProcess (shell cmd) { std_in = CreatePipe }
    writePbm pic hin >> waitForProcess ps
  put (em, tm, s)

addTrans :: TransformMatrix -> Interp
addTrans t = do
  (em, tm, s) <- get
  put (em, t `matMult` tm, s)

modEdges :: (EdgeMatrix -> EdgeMatrix) -> Interp
modEdges f = do
  (em, tm, s) <- get
  put (f em, tm , s)
