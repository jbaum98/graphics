{-# LANGUAGE ScopedTypeVariables #-}

module Language.MDL.Interp.Eval where

import Data.Maybe
import GHC.Prim
import Data.Primitive.ByteArray

import Data.ByteString.Lazy.Char8

import Data.Color
import Data.D2Point
import Data.D3Point
import Data.Matrix
import Data.Picture
import Language.MDL.Expr
import Language.MDL.Interp.Interp
import Language.MDL.SymTab

eval :: Expr -> Interp ()

eval (Set knob val) = modSymTab $ insert knob $ DoubleVal val


eval (Point (Triple x y z)) = addF (writePoint green (round x) (round y) z)

eval (Line _ p1 cs1 p2 cs2) = do
  tm1 <- getTM cs1
  tm2 <- getTM cs2
  let Triple x1 y1 z1 = transform tm1 p1
      Triple x2 y2 z2 = transform tm2 p2
  addF $ drawLine (round x1) (round y1) z1 (round x2) (round y2) z2

eval (Box k topLeft dims cs) = drawInCS cs k $ box topLeft dims

eval (Sphere k center r cs) = drawInCS cs k $ sphere center r 20

eval (Torus k center r1 r2 cs) = drawInCS cs k $ torus center r1 r2 30

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

eval Display = passPicTo displayPic

eval (Save path) = passPicTo $ savePic $ unpack path

eval (Light _ color loc) = addPointLight $ PointLight (truncate <$> color) loc

eval (Ambient color) = setAmbient $ truncate <$> color

eval (Constants name consts _) = modSymTab $ insert name $ ConstsVal consts

eval (Shading s) = setShading s

eval _ = return ()


-- |
-- = Misc Helpers

passPicTo :: (Picture RealWorld -> IO ()) -> Interp ()
passPicTo f = do
  pf <- gets picFunc
  pic <- blankPic (Pair 500 500)
  liftIO $ do
    pf pic
    f pic

scaledMat :: ByteString -> (D3Point -> TransformMatrix) -> D3Point -> Interp ()
scaledMat knob rotMat p =
  getKnob knob >>= multTop . rotMat . (*p) . pure

scaledRot :: ByteString -> (Double -> TransformMatrix) -> Double -> Interp ()
scaledRot knob rotMat degs =
  getKnob knob >>= multTop . rotMat . (*degs)

roundoff :: D3Point -> D2Point
roundoff (Triple x y _) = round <$> Pair x y
