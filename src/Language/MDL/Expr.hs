module Language.MDL.Expr (
  Expr(..),
  Axis(..),
  strToAxis,
  ShadingType(..),
  strToShadingType,
  Triple(..),
  ) where

import Pair

data Expr = Light { sym :: String, color ::  Triple Double, point :: Triple Double }
          | Move { point :: Triple Double, mKnob :: Maybe String }
          | Constants { sym :: String, lights :: Triple (Triple Double), colorIntensity :: Maybe (Triple Double) }
          | SaveCoords { sym :: String }
          | Camera { eye :: Triple Double, aim :: Triple Double }
          | Texture { sym :: String, d1 :: Triple Double, d2 :: Triple Double, d3 :: Triple Double, d4 :: Triple Double }
          | Sphere { consts :: Maybe String, center :: Triple Double, r :: Double, coordSystem :: Maybe String }
          | Torus { consts :: Maybe String, center :: Triple Double, r1 :: Double, r2 :: Double, coordSystem :: Maybe String }
          | Box { consts :: Maybe String, topLeft :: Triple Double, dims :: Triple Double, coordSystem :: Maybe String }
          | Line { consts :: Maybe String, p1 :: Triple Double, coordSystem1 :: Maybe String, p2 :: Triple Double, coordSystem2 :: Maybe String }
          | Mesh { consts :: Maybe String, filename :: FilePath, coordSystem :: Maybe String }
          | Set { knobname :: String, val :: Double }
          | Scale { scalars :: Triple Double, mKnob :: Maybe String }
          | Rotate { axis :: Axis, degs :: Double, mKnob :: Maybe String }
          | Basename { name :: String }
          | SaveKnobs { sym :: String }
          | Tween { startFrame :: Double, endFrame :: Double, knobs1 :: String, knobs2 :: String }
          | Frames { n :: Double }
          | Vary { knob :: String, startFrame :: Double, endFrame :: Double, startVal :: Double, endVal :: Double }
          | Push
          | GenerateRayfiles
          | Pop
          | Save { filename :: FilePath }
          | Shading { st :: ShadingType }
          | SetKnobs { val :: Double }
          | Focal { val :: Double }
          | Display
          | Web
          | Ambient { color :: Triple Double }
          | Comment
          deriving (Eq, Show)

data Axis = X | Y | Z deriving (Eq,Show)

strToAxis :: String -> Axis
strToAxis "x" = X
strToAxis "X" = X
strToAxis "y" = Y
strToAxis "Y" = Y
strToAxis "z" = Z
strToAxis "Z" = Z
strToAxis _   = error "Invalid rotation axis"

data ShadingType = Phong
                 | Flat
                 | Goroud
                 | Raytrace
                 | Wireframe
                 deriving (Eq, Show)

strToShadingType :: String -> ShadingType
strToShadingType "wireframe" = Wireframe
strToShadingType "flat"      = Flat
strToShadingType "goroud"    = Goroud
strToShadingType "phong"     = Phong
strToShadingType "raytrace"  = Raytrace
strToShadingType _   = error "Invalid shading type"
