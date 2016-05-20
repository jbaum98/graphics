{
module Language.MDL.Lexer (lexMDL) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Read
import Language.MDL.Tokens
}

%wrapper "posn-bytestring"


@id = [a-zA-Z][a-zA-Z0-9_]*

tokens :-

  $white+ ;

  \-?[0-9]+ |
  \-?[0-9]+\. |
  \-?[0-9]+\.[0-9]+ |
  \-?\.[0-9]+ {const $ TokenDouble . readDouble}

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

  phong|flat|goroud|raytrace|wireframe {const TokenShadingType}

  setknobs {\_ _ -> TokenSetKnobs}
  focal {\_ _ -> TokenFocal}
  display {\_ _ -> TokenDisplay}
  web {\_ _ -> TokenWeb}

  : {\_ _ -> TokenCO}

  [a-zA-Z][\.a-zA-Z0-9_]* {const TokenString}

{

lexMDL :: ByteString.ByteString -> [Token]
lexMDL = alexScanTokens

readDouble :: ByteString.ByteString -> Double
readDouble str | B.head str == '-' = read' $ '-' `B.cons` '0' `B.cons` B.tail str
               | otherwise     = read' $ '0' `B.cons` str
  where read' y = case signed fractional y of Just (x,_) -> x

}
