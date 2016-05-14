module Language.MDL.Interp where

import System.Process
import Data.Maybe
import Data.Foldable
import Control.Monad.State
import Prelude hiding (lookup)

import Matrix hiding (empty)
import Picture
import Pbm
import Shapes
import Language.MDL.Expr
import Language.MDL.SymTab hiding (fold)
import Forking

data ParseState = ParseState
  { picFunc        :: !(Picture -> Picture)
  , transStack     :: ![TransformMatrix]
  , maxP           :: !(Maybe D2Point)
  , symtab         :: !SymTab
  }

topTransformMat :: ParseState -> TransformMatrix
topTransformMat ps = case transStack ps of
                       (top:_) -> top
                       []      -> idMatrix

type Interp a = StateT ParseState IO a

execute :: Foldable f => f Expr -> IO ()
execute = evalInterp . mapM_ eval

evalInterp :: Interp () -> IO ()
evalInterp = flip evalStateT initState

initState :: ParseState
initState = ParseState id [] Nothing empty

getSym :: String -> Interp (Maybe Val)
getSym s = do
  symtab' <- gets symtab
  return $ lookup s symtab'

getSymWithDefault :: String -> Val -> Interp Val
getSymWithDefault s d = fromMaybe d <$> getSym s

eval :: Expr -> Interp ()

eval Comment = return ()

eval (Line _ p1 cs1 p2 cs2) = do
  tm1 <- getTM cs1
  tm2 <- getTM cs2
  let drawIt = drawLine (trans tm1 p1) (trans tm2 p2)
      trans tm = roundoff . transform tm
      roundoff (Triple x y _) = round <$> Pair x y
  modifyPicFunc (drawIt . )

eval (Box _ topLeft dims cs) = drawInCS topLeft cs $ \tl -> box tl dims

eval (Sphere _ center r cs) = drawInCS center cs $ \c -> sphere c r 100

eval (Torus _ center r1 r2 cs) = drawInCS center cs $ \c -> torus c r1 r2 100

eval (Scale scalars Nothing) = multTop $ scaleMatrix scalars
eval (Scale scalars (Just knob)) = do
  Just (DoubleVal knobVal) <- getSym knob
  multTop $ scaleMatrix $ pure knobVal * scalars

eval (Rotate X degs Nothing) = multTop $ rotXMatrix degs
eval (Rotate Y degs Nothing) = multTop $ rotYMatrix degs
eval (Rotate Z degs Nothing) = multTop $ rotZMatrix degs
eval (Rotate X degs (Just knob)) =
  getKnob knob >>= multTop . rotXMatrix . (*degs)
eval (Rotate Y degs (Just knob)) =
  getKnob knob >>= multTop . rotYMatrix . (*degs)
eval (Rotate Z degs (Just knob)) =
  getKnob knob >>= multTop . rotZMatrix . (*degs)

eval (Move trans Nothing) = multTop $ transMatrix trans
eval (Move trans (Just knob)) =
  getKnob knob >>= multTop . transMatrix . (* trans) . pure

eval Push = modifyTransStack pushF
  where pushF (oldTM:rest) = oldTM : oldTM : rest
        pushF [] = [idMatrix]

eval Pop = modifyTransStack popF
  where popF (_:rest) = rest
        popF [] = []

eval Display = writePicToProcess "display"

eval (Save path) = writePicToProcess $ "convert - " ++ path

drawInCS :: ShapeMatrix m => D3Point -> Maybe String -> (D3Point -> m) -> Interp ()
drawInCS p cs f = do
  tm <- getTM cs
  let p' = transform tm p
  drawShape $ f p'

getTM :: Maybe String -> Interp TransformMatrix
getTM (Just cs) = do
  mknob <- getSym cs
  case mknob of
    Just val -> case val of
      MatrixVal tm -> return tm
      _            -> failKnobType "matrix"
    Nothing  -> failNoKnob cs
getTM Nothing = gets topTransformMat

getKnob :: String -> Interp Double
getKnob knob = do
  mknob <- getSym knob
  case mknob of
    Just val -> case val of
      DoubleVal d -> return d
      _           -> failKnobType "double"
    Nothing -> failNoKnob knob

failKnobType :: String -> Interp a
failKnobType t = fail $ "Provided knob is not of type `" ++ t ++ "`"

failNoKnob :: String -> Interp a
failNoKnob knob = fail $ "Knob `" ++ knob ++ "` is not initialized"

drawShape :: ShapeMatrix m => m -> Interp ()
drawShape em = modify f
  where f st = st { picFunc = draw shape . picFunc st }
          where shape = topTransformMat st `matMultD` em

modifyPicFunc :: ((Picture -> Picture) -> (Picture -> Picture)) -> Interp ()
modifyPicFunc f = modify $ \st -> st { picFunc = f $ picFunc st }

modifyTransStack :: ([TransformMatrix] -> [TransformMatrix]) -> Interp ()
modifyTransStack f = modify $ \st -> st { transStack = f $ transStack st }

modifyTopTrans :: (TransformMatrix -> TransformMatrix) -> Interp ()
modifyTopTrans f = modifyTransStack $ \tstack ->
  case tstack of
    (tm:rest) -> f tm : rest
    []        -> [f idMatrix]

multTop :: TransformMatrix -> Interp ()
multTop = modifyTopTrans . matMult

writePicToProcess :: String -> Interp ()
writePicToProcess cmd = do
  f <- gets picFunc
  s' <- gets maxP
  let pic = f $ blankPic $ fromMaybe (Pair 500 500) s'
  void . liftIO . forkChild $ do
    (Just hin, _, _, ps) <-
      createProcess (shell cmd) { std_in = CreatePipe }
    void $ writePbm pic hin >> waitForProcess ps
