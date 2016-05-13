{-# LANGUAGE TupleSections #-}

module Parser.Interp (
  execute,
  runEval,
  eval
  ) where

import Picture
import Pbm
import Matrix
import Parser.Parser
import Control.Monad.State
import System.Process
import Shapes

type PicFunc = Picture -> Picture
data ParseState = ParseState { pf :: PicFunc, tms :: [TransformMatrix], s :: D2Point }

tm :: ParseState -> TransformMatrix
tm = head . tms

type Interp = StateT ParseState IO ()

execute :: D2Point -> [Command] -> IO ()
execute sizeP = runEval sizeP . mapM_ eval

runEval :: D2Point -> Interp -> IO ()
runEval = flip evalStateT . initState

initState :: D2Point -> ParseState
initState = ParseState id []

-- eval :: Command -> ParseState -> ((IO (), ParseState))
eval :: Command -> Interp
eval (Line x0 y0 z0 x1 y1 z1) = drawShape $ edge p0 p1
  where
    p0 = Triple x0 y0 z0
    p1 = Triple x1 y1 z1

eval (Circle cx cy r) = drawShape $ circle 100 center r
  where center = Triple cx cy 0

eval (Hermite x0 y0 x1 y1 x2 y2 x3 y3) = drawShape $ hermite 100 p0 r0 p1 r1
  where
   p0   = Triple x0 y0 0
   p1   = Triple x2 y2 0
   ctl0 = Triple x1 y1 0
   ctl1 = Triple x3 y3 0
   r0   = ctl0 - p0
   r1   = ctl1 - p1

eval (Bezier x1 y1 x2 y2 x3 y3 x4 y4) = drawShape $ bezier 100 p1 p2 p3 p4
  where
    p1  = Triple x1 y1 0
    p2  = Triple x2 y2 0
    p3  = Triple x3 y3 0
    p4  = Triple x4 y4 0

eval (Box x y z w h d) = drawShape $ box (Triple x y z) (Triple w h d)

eval (Torus x y r1 r2) = drawShape $ torus (Triple x y 0) r1 r2 11

eval (Sphere x y r) = drawShape $ sphere (Triple x y 0) r 11

eval (Scale sx sy sz) = addTrans . scaleMatrix $ Triple sx sy sz

eval (Translate tx ty tz) = addTrans . transMatrix $ Triple tx ty tz

eval (RotateX degs) = addTrans $ rotXMatrix degs

eval (RotateY degs) = addTrans $ rotYMatrix degs

eval (RotateZ degs) = addTrans $ rotZMatrix degs

eval Push = modifyTrans pushF
  where pushF (oldTM:rest) = oldTM : oldTM : rest
        pushF [] = [idMatrix]

eval Pop = modifyTrans popF
  where popF (_:rest) = rest
        popF [] = []

eval Display = writePicToProcess "display"

eval (Save path) = writePicToProcess $ "convert - " ++ path

writePicToProcess :: String -> Interp
writePicToProcess cmd = do
  f <- gets pf
  s' <- gets s
  let pic = f $ blankPic s'
  void . liftIO $ do
    (Just hin, _, _, ps) <-
      createProcess (shell cmd) { std_in = CreatePipe }
    writePbm pic hin >> waitForProcess ps

modifyTrans :: ([TransformMatrix] -> [TransformMatrix]) -> Interp
modifyTrans f = modify $ \st -> st { tms = f $ tms st }

addTrans :: TransformMatrix -> Interp
addTrans newTM = modifyTrans modF
  where modF (oldTM:rest) = (oldTM `matMult` newTM) : rest
        modF [] = [newTM] -- shouldn't happen

drawShape :: ShapeMatrix m => m  -> Interp
drawShape em = modify $ \st -> st { pf = draw (tm st `matMultD` em) . pf st }
