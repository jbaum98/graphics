module Language.MDL.Interp.Interp (
  Interp,
  runInterp,
  evalInterp,
  execInterp,
  get,
  put,
  gets,
  modify,
  liftIO,
  module Language.MDL.Interp.InterpState,
  failKnobType,
  failNoKnob,
  getSym,
  getTM,
  getKnob,
  modPicFunc,
  addF,
  modTransStack,
  modTopTrans,
  multTop,
  drawInCS
  ) where

import Control.Monad.State
import Data.Maybe
import Prelude hiding (lookup)

import Language.MDL.Interp.InterpState
import Language.MDL.SymTab hiding (fold)
import Matrix hiding (empty)
import Picture

-- |
-- = Interp Monad

type Interp a = StateT InterpState IO a

runInterp :: Interp a -> InterpState -> IO (a, InterpState)
runInterp = runStateT

evalInterp :: Interp a -> InterpState -> IO a
evalInterp = evalStateT

execInterp :: Interp a -> InterpState -> IO InterpState
execInterp = execStateT

-- |
-- == Specific fail messages

failKnobType :: String -> Interp a
failKnobType t = fail $ "Provided knob is not of type `" ++ t ++ "`"

failNoKnob :: String -> Interp a
failNoKnob knob = fail $ "Knob `" ++ knob ++ "` is not initialized"

-- |
-- == Accessing InterpState

getSym :: String -> Interp (Maybe Val)
getSym s = lookup s <$> gets symtab

getTM :: Maybe String -> Interp TransformMatrix
getTM (Just cs) = do
  mknob <- getSym cs
  case mknob of
    Just val -> case val of
      MatrixVal tm -> return tm
      _            -> failKnobType "matrix"
    Nothing  -> failNoKnob cs
getTM Nothing = gets topTransMat

getKnob :: String -> Interp Double
getKnob knob = do
  mknob <- getSym knob
  case mknob of
    Just val -> case val of
      DoubleVal d -> return d
      _           -> failKnobType "double"
    Nothing -> failNoKnob knob

-- |
-- == Modifying InterpState

modPicFunc :: ((Picture -> Picture) -> (Picture -> Picture)) -> Interp ()
modPicFunc f = modify $ \st -> st { picFunc = f $ picFunc st }

addF :: (Picture -> Picture) -> Interp ()
addF = modPicFunc . (.)

modTransStack :: ([TransformMatrix] -> [TransformMatrix]) -> Interp ()
modTransStack f = modify $ \st -> st { transStack = f $ transStack st }

modTopTrans :: (TransformMatrix -> TransformMatrix) -> Interp ()
modTopTrans f = modTransStack $ \tstack ->
  case tstack of
    (tm:rest) -> f tm : rest
    []        -> [f idMatrix]

multTop :: TransformMatrix -> Interp ()
multTop = modTopTrans . flip matMult

-- |
-- == Drawing shapes

drawShape :: ShapeMatrix m => m -> Interp ()
drawShape = addF . draw

drawInCS :: ShapeMatrix m => Maybe String -> m -> Interp ()
drawInCS cs m = getTM cs >>= drawShape . flip matMultD m
