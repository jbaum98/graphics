{-# LANGUAGE TupleSections #-}

module Parser.Interp (
  execute,
  runEval,
  eval
  ) where

import Picture
import Pbm
import Matrix hiding ((++))
import Parser.Parser
import Control.Monad.State
import System.Process
import System.IO

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
eval (Line x0 y0 z0 x1 y1 z1) = do
  (em, tm, s) <- get
  let em' = addEdge (Triple x0 y0 z0) (Triple x1 y1 z1) em
  put (em', tm, s)

eval (Circle cx cy r) = undefined

eval (Hermite x0 y0 dx0 dy0 x1 y1 dx1 dy1) = undefined

eval (Bezier x0 y0 x1 y1 x2 y2 x3 y3) = undefined

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
  liftIO $ do
    (Just hin, _, _, ps) <-
      createProcess (shell cmd) { std_in = CreatePipe }
    writePbm pic hin >> waitForProcess ps
  put (em, tm, s)

addTrans :: TransformMatrix -> Interp
addTrans t = do
  (em, tm, s) <- get
  put (em, t `matMult` tm, s)
