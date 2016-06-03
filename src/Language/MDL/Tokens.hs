module Language.MDL.Tokens (Token(..)) where

import Data.ByteString.Lazy (ByteString)

data Token = TokenDouble !Double
           | TokenComment
           | TokenLight
           | TokenConstants
           | TokenSaveCoords
           | TokenCamera
           | TokenAmbient
           | TokenTorus
           | TokenSphere
           | TokenBox
           | TokenLine
           | TokenPoint
           | TokenMesh
           | TokenTexture
           | TokenSet
           | TokenMove
           | TokenScale
           | TokenRotate
           | TokenBasename
           | TokenSaveKnobs
           | TokenTween
           | TokenFrames
           | TokenVary
           | TokenPush
           | TokenPop
           | TokenSave
           | TokenGenerateRayfiles
           | TokenShading
           | TokenShadingType ByteString
           | TokenSetKnobs
           | TokenFocal
           | TokenDisplay
           | TokenWeb
           | TokenCO
           | TokenString ByteString
           deriving (Eq, Show)
