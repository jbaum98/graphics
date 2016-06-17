{-# LANGUAGE BangPatterns #-}

{-|
Module      : Language.MDL.Interp.Interp
Description : Interp monad and utility methods

Contains the Interp monad, which is used to store the state of the interpreter
and execute actions, as well as useful functions for accessing and modifying
that state. The state is stored using 'InterpState'.
-}

module Language.MDL.Interp.Interp (
  Interp,
  -- * State-like Functions
  runInterp,
  evalInterp,
  execInterp,
  -- * Useful State Functions
  get,
  put,
  gets,
  modify,
  liftIO,
  -- * InterpState
  InterpState(..),
  PointLight(PointLight),
  initState,
  -- * Specialized Failure Messages
  failKnobType,
  failNoKnob,
  -- * Accessors
  getSym,
  getTM,
  getKnob,
  -- * Modifiers
  modPicFunc,
  addF,
  modTransStack,
  modTopTrans,
  multTop,
  setAmbient,
  addPointLight,
  setShading,
  modSymTab,
  drawInCS
  ) where

import Data.Maybe
import Prelude hiding (lookup)
import GHC.Prim

import Control.Monad.State.Strict
import Data.ByteString.Lazy.Char8

import Data.Picture.Drawing
import Data.Color
import Data.Matrix hiding (empty)
import Data.Picture
import Language.MDL.Interp.InterpState
import Language.MDL.SymTab hiding (fold)

-- | A state monad that produces 'IO' actions, using 'InterpState'
type Interp a = StateT InterpState IO a

-- | Given an 'Interp' and a starting 'InterpState', produces an action giving a result and the next state
runInterp :: Interp a -> InterpState -> IO (a, InterpState)
runInterp = runStateT

-- | Given an 'Interp' and a starting 'InterpState', produces an action giving a result
evalInterp :: Interp a -> InterpState -> IO a
evalInterp = evalStateT

-- | Given an 'Interp' and a starting 'InterpState', produces an action giving the next state
execInterp :: Interp a -> InterpState -> IO InterpState
execInterp = execStateT

-- | Fails with an error message for an incorrect knob type
failKnobType :: String -> Interp a
failKnobType t = fail $ "Provided knob is not of type `" ++ t ++ "`"

-- | Fails with an error message for a missing knob
failNoKnob :: ByteString -> Interp a
failNoKnob knob = fail $ "Knob `" ++ unpack knob ++ "` is not initialized"

-- | Get a value from the symbol table, if it exists
getSym :: ByteString -> Interp (Maybe Val)
getSym s = lookup s <$> gets symtab

-- | Get a named 'TransformMatrix' from the symbol table, or fail. If the
-- provided name is 'Nothing', use the top of the transformation stack.
getTM :: Maybe ByteString -> Interp TransformMatrix
getTM (Just cs) = do
  mknob <- getSym cs
  case mknob of
    Just val -> case val of
      MatrixVal tm -> return tm
      _            -> failKnobType "matrix"
    Nothing  -> failNoKnob cs
getTM Nothing = gets topTransMat

-- | Get the value of a named knob from the symbol table, or fail
getKnob :: ByteString -> Interp Double
getKnob knob = do
  mknob <- getSym knob
  case mknob of
    Just val -> case val of
      DoubleVal d -> return d
      _           -> failKnobType "double"
    Nothing -> failNoKnob knob

-- | Get the value of a named set of constants from the symbol table, or fail.
-- If the provided name is 'Nothing', use a set of default lighting constants.
getConsts :: Maybe ByteString -> Interp LightingConsts
getConsts (Just knob) = do
  mknob <- getSym knob
  case mknob of
    Just val -> case val of
      ConstsVal k -> return k
      _           -> failKnobType "constants"
    Nothing  -> failNoKnob knob
getConsts Nothing = return defaultLightingConsts

-- | Apply a function to the current picture function
modPicFunc :: ((Picture RealWorld -> IO ()) -> (Picture RealWorld -> IO ())) -> Interp ()
modPicFunc f = modify $ \st -> st { picFunc = f $ picFunc st }

-- | Append a picture function to the current one, so it is executed after
addF :: (Picture RealWorld -> IO ()) -> Interp ()
addF newF = modPicFunc $ \oldF ->
                           \pic -> do
                             oldF pic
                             newF pic

-- | Apply a function to the transformation stack
modTransStack :: ([TransformMatrix] -> [TransformMatrix]) -> Interp ()
modTransStack f = modify $ \st -> st { transStack = f $ transStack st }

-- | Apply a function to the topmost element of the transformation stack
modTopTrans :: (TransformMatrix -> TransformMatrix) -> Interp ()
modTopTrans f = modTransStack $ \tstack ->
  case tstack of
    (tm:rest) -> f tm : rest
    []        -> [f idMatrix]

-- | Multiply the top of the transformation stack by some 'TransforMatrix'
multTop :: TransformMatrix -> Interp ()
multTop = modTopTrans . flip matMult

-- | Apply a function to the symbol table
modSymTab :: (SymTab -> SymTab) -> Interp ()
modSymTab f = modify $ \st -> st { symtab = f $ symtab st }

-- | Apply a function to the lighting information
modLighting :: (Lighting -> Lighting) -> Interp ()
modLighting f = modify $ \st -> st { lighting = f $ lighting st }

-- | Set the ambient light level
setAmbient :: Color -> Interp ()
setAmbient color = modLighting $ \lighting -> lighting { ambient = color }

-- | Add a point light source to the list of point light sources
addPointLight :: PointLight -> Interp ()
addPointLight light = modLighting $ \lighting -> lighting { lights = light:(lights lighting) }

-- | Set the shading type
setShading :: ShadingType -> Interp ()
setShading s = modify $ \st -> st { shading = s }

-- | Draw a shape, by appending it to the picture function
drawShape :: ShapeMatrix m
          => Maybe ByteString -- ^ The name of the constants
          -> m                -- ^ The drawable 'ShapeMatrix'
          -> Interp ()
drawShape kname shape = do
  l <- gets lighting
  k <- getConsts kname
  s <- gets shading
  addF $ draw (l,k) s shape
{-# INLINE drawShape #-}

-- | Draw a shape in a coordinate system
drawInCS :: ShapeMatrix m
         => Maybe ByteString -- ^ The name of the coordinate system
         -> Maybe ByteString -- ^ The name of the constants
         -> m                -- ^ The drawable 'ShapeMatrix'
         -> Interp ()
drawInCS cs kname m = do
  !tm <- getTM cs
  drawShape kname $! tm `matMultD` m
{-# INLINE drawInCS #-}
