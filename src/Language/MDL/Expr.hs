{-# LANGUAGE OverloadedStrings #-}

module Language.MDL.Expr (
  Expr(..),
  Axis(..),
  strToAxis,
  ShadingType(..),
  strToShadingType,
  Triple(..),
  ) where

import Data.ByteString.Lazy

import Pair

data Expr = Light ByteString (Triple Double) (Triple Double)
          | Move (Triple Double) (Maybe ByteString)
          | Constants ByteString (Triple (Triple Double)) (Maybe (Triple Double))
          | SaveCoords ByteString
          | Camera (Triple Double) (Triple Double)
          | Texture ByteString (Triple Double) (Triple Double) (Triple Double) (Triple Double)
          | Sphere (Maybe ByteString) (Triple Double) Double (Maybe ByteString)
          | Torus (Maybe ByteString) (Triple Double) Double Double (Maybe ByteString)
          | Box (Maybe ByteString) (Triple Double) (Triple Double) (Maybe ByteString)
          | Line (Maybe ByteString) (Triple Double) (Maybe ByteString) (Triple Double) (Maybe ByteString)
          | Mesh (Maybe ByteString) ByteString (Maybe ByteString)
          | Set ByteString Double
          | Scale (Triple Double) (Maybe ByteString)
          | Rotate Axis Double (Maybe ByteString)
          | Basename ByteString
          | SaveKnobs ByteString
          | Tween Double Double ByteString ByteString
          | Frames Double
          | Vary ByteString Double Double Double Double
          | Push
          | Pop
          | GenerateRayfiles
          | Save ByteString
          | Shading ShadingType
          | SetKnobs Double
          | Focal Double
          | Display
          | Web
          | Ambient (Triple Double)
          | Comment
          deriving (Eq, Show)

data Axis = X | Y | Z deriving (Eq, Show)

strToAxis :: ByteString -> Axis
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

strToShadingType :: ByteString -> ShadingType
strToShadingType "wireframe" = Wireframe
strToShadingType "flat"      = Flat
strToShadingType "goroud"    = Goroud
strToShadingType "phong"     = Phong
strToShadingType "raytrace"  = Raytrace
strToShadingType _   = error "Invalid shading type"
