module Language.MDL.Tokens (Token(..)) where

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
           | TokenShadingType String
           | TokenSetKnobs
           | TokenFocal
           | TokenDisplay
           | TokenWeb
           | TokenCO
           | TokenString String
           deriving (Eq, Show)
