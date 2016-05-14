module Language.MDL.Expr (
  Expr(..),
  Axis(..),
  strToAxis,
  ShadingType(..),
  strToShadingType,
  Triple(..),
  ) where

import Pair

data Expr = Light String (Triple Double) (Triple Double)
          | Move (Triple Double) (Maybe String)
          | Constants String (Triple (Triple Double)) (Maybe (Triple Double))
          | SaveCoords String
          | Camera (Triple Double) (Triple Double)
          | Texture String (Triple Double) (Triple Double) (Triple Double) (Triple Double)
          | Sphere (Maybe String) (Triple Double) Double (Maybe String)
          | Torus (Maybe String) (Triple Double) Double Double (Maybe String)
          | Box (Maybe String) (Triple Double) (Triple Double) (Maybe String)
          | Line (Maybe String) (Triple Double) (Maybe String) (Triple Double) (Maybe String)
          | Mesh (Maybe String) FilePath (Maybe String)
          | Set String Double
          | Scale (Triple Double) (Maybe String)
          | Rotate Axis Double (Maybe String)
          | Basename String
          | SaveKnobs String
          | Tween Double Double String String
          | Frames Double
          | Vary String Double Double Double Double
          | Push
          | Pop
          | GenerateRayfiles
          | Save FilePath
          | Shading ShadingType
          | SetKnobs Double
          | Focal Double
          | Display
          | Web
          | Ambient (Triple Double)
          | Comment
          deriving (Eq, Show)

data Axis = X | Y | Z deriving (Eq, Show)

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
