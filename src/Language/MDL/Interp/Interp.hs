{-# LANGUAGE BangPatterns #-}

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
  --module Language.MDL.Interp.InterpState,
  PointLight(PointLight),
  InterpState(..),
  initState,
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
  setAmbient,
  addPointLight,
  modSymTab,
  drawInCS
  ) where

import Data.Maybe
import Prelude hiding (lookup)
import Control.Monad
import GHC.Prim

import Control.Monad.State.Strict
import Data.ByteString.Lazy.Char8

import Data.Picture.Drawing.Lighting
import Data.Color
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

getConsts :: Maybe ByteString -> Interp LightingConsts
getConsts (Just knob) = do
  mknob <- getSym knob
  case mknob of
    Just val -> case val of
      ConstsVal k -> return k
      _           -> failKnobType "constants"
    Nothing  -> failNoKnob knob
getConsts Nothing = return defaultLightingConsts

-- |
-- == Modifying InterpState

modPicFunc :: ((Picture RealWorld -> IO ()) -> (Picture RealWorld -> IO ())) -> Interp ()
modPicFunc f = modify $ \st -> st { picFunc = f $ picFunc st }

addF :: (Picture RealWorld -> IO ()) -> Interp ()
addF newF = modPicFunc $ \oldF ->
                           \pic -> do
                             oldF pic
                             newF pic

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

modLighting :: (Lighting -> Lighting) -> Interp ()
modLighting f = modify $ \st -> st { lighting = f $ lighting st }

setAmbient :: Color -> Interp ()
setAmbient color = modLighting $ \lighting -> lighting { ambient = color }

addPointLight :: PointLight -> Interp ()
addPointLight light = modLighting $ \lighting -> lighting { lights = light:(lights lighting) }

-- |
-- == Drawing shapes

drawShape :: ShapeMatrix m => Maybe ByteString -> m -> Interp ()
drawShape kname shape = do
  l <- gets lighting
  k <- getConsts kname
  addF $ draw (l,k) shape
{-# INLINE drawShape #-}

drawInCS :: ShapeMatrix m => Maybe ByteString -> Maybe ByteString -> m -> Interp ()
drawInCS cs kname m = do
  !tm <- getTM cs
  drawShape kname $! tm `matMultD` m
{-# INLINE drawInCS #-}
