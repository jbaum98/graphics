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
  \-?\.[0-9]+ {const $ Double . read}

  "#".* {\_ _ -> Comment}
  "//".* {\_ _ -> Comment}

  light {\_ _ -> Light}
  const . constants {\_ _ -> Constants}
  save_coord_system {\_ _ -> SaveCoords}
  camera {\_ _ -> Camera}
  ambient {\_ _ -> Ambient}

  torus {\_ _ -> Torus}
  sphere {\_ _ -> Sphere}
  box {\_ _ -> Box}
  line {\_ _ -> Line}
  mesh {\_ _ -> Mesh}
  texture {\_ _ -> Texture}

  set {\_ _ -> Set}
  move {\_ _ -> Move}
  scale {\_ _ -> Scale}
  rotate {\_ _ -> Rotate}
  basename {\_ _ -> Basename}
  save_knobs {\_ _ -> SaveKnobs}
  tween {\_ _ -> Tween}
  frames {\_ _ -> Frames}
  vary {\_ _ -> Vary}

  push {\_ _ -> Push}
  pop {\_ _ -> Pop}
  save {\_ _ -> Save}
  generate_rayfiles {\_ _ -> GenerateRayfiles}

  shading {\_ _ -> Shading}

  phong|flat|goroud|raytrace|wireframe {
    \_ s -> let st = case s of
                      "phong" -> Phong
                      "flat" -> Flat
                      "goroud" -> Goroud
                      "raytrace" -> Raytrace
                      "Wireframe" -> Wireframe
            in ShadingTypeToken st}

  setknobs {\_ _ -> SetKnobs}
  focal {\_ _ -> Focal}
  display {\_ _ -> Display}
  web {\_ _ -> Web}

  : {\_ _ -> CO}

  [a-zA-Z][\.a-zA-Z0-9_]* {const String}

{

data Token = Double !Double
           | Comment
           | Light
           | Constants
           | SaveCoords
           | Camera
           | Ambient
           | Torus
           | Sphere
           | Box
           | Line
           | Mesh
           | Texture
           | Set
           | Move
           | Scale
           | Rotate
           | Basename
           | SaveKnobs
           | Tween
           | Frames
           | Vary
           | Push
           | Pop
           | Save
           | GenerateRayfiles
           | Shading
           | ShadingTypeToken ShadingType
           | SetKnobs
           | Focal
           | Display
           | Web
           | CO
           | String String
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
