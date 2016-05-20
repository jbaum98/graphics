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
  modSymTab,
  drawInCS
  ) where

import Data.Maybe
import Prelude hiding (lookup)

import Control.Monad.State.Strict
import Data.ByteString.Lazy.Char8

import Data.Matrix hiding (empty)
import Data.Picture
import Language.MDL.Interp.InterpState
import Language.MDL.SymTab hiding (fold)

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

failNoKnob :: ByteString -> Interp a
failNoKnob knob = fail $ "Knob `" ++ unpack knob ++ "` is not initialized"

-- |
-- == Accessing InterpState

getSym :: ByteString -> Interp (Maybe Val)
getSym s = lookup s <$> gets symtab

getTM :: Maybe ByteString -> Interp TransformMatrix
getTM (Just cs) = do
  mknob <- getSym cs
  case mknob of
    Just val -> case val of
      MatrixVal tm -> return tm
      _            -> failKnobType "matrix"
    Nothing  -> failNoKnob cs
getTM Nothing = gets topTransMat

getKnob :: ByteString -> Interp Double
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


modSymTab :: (SymTab -> SymTab) -> Interp ()
modSymTab f = modify $ \st -> st { symtab = f $ symtab st }

-- |
-- == Drawing shapes

drawShape :: ShapeMatrix m => m -> Interp ()
drawShape = addF . draw

drawInCS :: ShapeMatrix m => Maybe ByteString -> m -> Interp ()
drawInCS cs m = getTM cs >>= drawShape . flip matMultD m
