{
module MDL (scanTokens) where
}

%wrapper "posn"


@id = [a-zA-Z][a-zA-Z0-9_]*

tokens :-

  $white+ ;

  \-?[0-9]+ |
  \-?[0-9]+\. |
  \-?[0-9]+\.[0-9]+ |
  \-?\.[0-9]+ {const $ TokenDouble . read}

  "#".* {\_ _ -> TokenComment}
  "//".* {\_ _ -> TokenComment}

  light {\_ _ -> TokenLight}
  constants {\_ _ -> TokenConstants}
  save_coord_system {\_ _ -> TokenSaveCoords}
  camera {\_ _ -> TokenCamera}
  ambient {\_ _ -> TokenAmbient}

  torus {\_ _ -> TokenTorus}
  sphere {\_ _ -> TokenSphere}
  box {\_ _ -> TokenBox}
  line {\_ _ -> TokenLine}
  mesh {\_ _ -> TokenMesh}
  texture {\_ _ -> TokenTexture}

  set {\_ _ -> TokenSet}
  move {\_ _ -> TokenMove}
  scale {\_ _ -> TokenScale}
  rotate {\_ _ -> TokenRotate}
  basename {\_ _ -> TokenBasename}
  save_knobs {\_ _ -> TokenSaveKnobs}
  tween {\_ _ -> TokenTween}
  frames {\_ _ -> TokenFrames}
  vary {\_ _ -> TokenVary}

  push {\_ _ -> TokenPush}
  pop {\_ _ -> TokenPop}
  save {\_ _ -> TokenSave}
  generate_rayfiles {\_ _ -> TokenGenerateRayfiles}

  shading {\_ _ -> TokenShading}

  phong|flat|goroud|raytrace|wireframe {
    \_ s -> let st = case s of
                      "phong" -> Phong
                      "flat" -> Flat
                      "goroud" -> Goroud
                      "raytrace" -> Raytrace
                      "Wireframe" -> Wireframe
            in TokenShadingType st}

  setknobs {\_ _ -> TokenSetKnobs}
  focal {\_ _ -> TokenFocal}
  display {\_ _ -> TokenDisplay}
  web {\_ _ -> TokenWeb}

  : {\_ _ -> TokenCO}

  [a-zA-Z][\.a-zA-Z0-9_]* {const TokenString}

{

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
           | TokenShadingType ShadingType
           | TokenSetKnobs
           | TokenFocal
           | TokenDisplay
           | TokenWeb
           | TokenCO
           | TokenString String
           deriving (Eq, Show)

data ShadingType = Phong
                 | Flat
                 | Goroud
                 | Raytrace
                 | Wireframe
                 deriving (Eq, Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

}
