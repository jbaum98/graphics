{
module Language.MDL.Parser (parse, Syntax(..)) where

import Language.MDL.Tokens
import Language.MDL.Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    COMMENT { TokenComment }
    DOUBLE { TokenDouble $$ }
    LIGHT { TokenLight }
    AMBIENT { TokenAmbient }
    CONSTANTS { TokenConstants }
    SAVE_COORDS { TokenSaveCoords }
    CAMERA { TokenCamera }
    SPHERE { TokenSphere }
    TORUS { TokenTorus }
    BOX { TokenBox }
    LINE { TokenLine }
    MESH { TokenMesh }
    TEXTURE { TokenTexture }
    STRING { TokenString $$ }
    SET { TokenSet }
    MOVE { TokenMove }
    SCALE { TokenScale }
    ROTATE { TokenRotate }
    BASENAME { TokenBasename }
    SAVE_KNOBS { TokenSaveKnobs }
    TWEEN { TokenTween }
    FRAMES { TokenFrames }
    VARY { TokenVary }
    PUSH { TokenPush }
    POP { TokenPop }
    SAVE { TokenSave }
    GENERATE_RAYFILES{ TokenGenerateRayfiles }
    SHADING { TokenShading }
    SHADING_TYPE { TokenShadingType $$ }
    SETKNOBS { TokenSetKnobs }
    FOCAL { TokenFocal }
    DISPLAY { TokenDisplay }
    WEB { TokenWeb }
    CO { TokenCO }
%%

commands :: { [Syntax] }
         : {- empty -}         { [] }
         | commands command    { $2 : $1 }

command :: { Syntax }
        : COMMENT { Comment }
        | LIGHT STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE { Light $2 (Triple $3 $4 $5) (Triple $6 $7 $8) }
        | MOVE DOUBLE DOUBLE DOUBLE STRING                       { Move (Triple $2 $3 $4) (Just $5) }
        | MOVE DOUBLE DOUBLE DOUBLE                              { Move (Triple $2 $3 $4) Nothing }
        | CONSTANTS STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE {
            Constants $2 (Triple (Triple $3 $4 $5) (Triple $6 $7 $8) (Triple $9 $10 $11)) Nothing }
        | CONSTANTS STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE {
            Constants $2 (Triple (Triple $3 $4 $5) (Triple $6 $7 $8) (Triple $9 $10 $11)) (Just (Triple $12 $13 $14)) }
        | SAVE_COORDS STRING                                     { SaveCoords $2 }
        | CAMERA DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE       { Camera (Triple $2 $3 $4) (Triple $5 $6 $7) }
        | TEXTURE STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE {
            Texture $2 (Triple $3 $4 $5) (Triple $6 $7 $8) (Triple $9 $10 $11) (Triple $12 $13 $14) }
        | SPHERE DOUBLE DOUBLE DOUBLE DOUBLE                     { Sphere Nothing (Triple $2 $3 $4) $5 Nothing }
        | SPHERE DOUBLE DOUBLE DOUBLE DOUBLE STRING              { Sphere Nothing (Triple $2 $3 $4) $5 (Just $6) }
        | SPHERE STRING DOUBLE DOUBLE DOUBLE DOUBLE              { Sphere (Just $2) (Triple $3 $4 $5) $6 Nothing }
        | SPHERE STRING DOUBLE DOUBLE DOUBLE DOUBLE STRING       { Sphere (Just $2) (Triple $3 $4 $5) $6 (Just $7) }
        | TORUS DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE               { Torus Nothing (Triple $2 $3 $4) $5 $6 Nothing }
        | TORUS DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING        { Torus Nothing (Triple $2 $3 $4) $5 $6 (Just $7) }
        | TORUS STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE        { Torus (Just $2) (Triple $3 $4 $5) $6 $7 Nothing }
        | TORUS STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING { Torus (Just $2) (Triple $3 $4 $5) $6 $7 (Just $8) }
        | BOX DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE          { Box Nothing (Triple $2 $3 $4) (Triple $5 $6 $7) Nothing}
        | BOX DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING   { Box Nothing (Triple $2 $3 $4) (Triple $5 $6 $7) (Just $8) }
        | BOX STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE   { Box (Just $2) (Triple $3 $4 $5) (Triple $6 $7 $8) Nothing }
        | BOX STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING {
            Box (Just $2) (Triple $3 $4 $5) (Triple $6 $7 $8) (Just $9) }
        | LINE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE         { Line Nothing (Triple $2 $3 $4) Nothing (Triple $5 $6 $7) Nothing }
        {- first do cs0, then cs1, then both - BUT NO CONSTANTS -}
        | LINE DOUBLE DOUBLE DOUBLE STRING DOUBLE DOUBLE DOUBLE  { Line Nothing (Triple $2 $3 $4) (Just $5) (Triple $6 $7 $8) Nothing }
        | LINE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING  { Line Nothing (Triple $2 $3 $4) Nothing (Triple $5 $6 $7) (Just $8) }
        | LINE DOUBLE DOUBLE DOUBLE STRING DOUBLE DOUBLE DOUBLE STRING {
            Line Nothing (Triple $2 $3 $4) (Just $5) (Triple $6 $7 $8) (Just $9) }
        {- now do constants, and constants with the cs stuff -}
        | LINE STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE  { Line (Just $2) (Triple $3 $4 $5) Nothing (Triple $6 $7 $8) Nothing }
        | LINE STRING DOUBLE DOUBLE DOUBLE STRING DOUBLE DOUBLE DOUBLE {
            Line (Just $2) (Triple $3 $4 $5) (Just $6) (Triple $7 $8 $9) Nothing }
        | LINE STRING DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE DOUBLE STRING {
            Line (Just $2) (Triple $3 $4 $5) Nothing (Triple $6 $7 $8) (Just $9) }
        | LINE STRING DOUBLE DOUBLE DOUBLE STRING DOUBLE DOUBLE DOUBLE STRING {
            Line (Just $2) (Triple $3 $4 $5) (Just $6) (Triple $7 $8 $9) (Just $10) }
        | MESH CO STRING                                         { Mesh Nothing $3 Nothing }
        | MESH STRING CO STRING {- name and constants -}         { Mesh (Just $2) $4 Nothing }
        | MESH STRING CO STRING STRING                           { Mesh (Just $2) $4 (Just $5)}
        | SET STRING DOUBLE                                      { Set $2 $3 }
        | SCALE DOUBLE DOUBLE DOUBLE STRING                      { Scale (Triple $2 $3 $4) (Just $5) }
        | SCALE DOUBLE DOUBLE DOUBLE                             { Scale (Triple $2 $3 $4) Nothing }
        | ROTATE STRING DOUBLE STRING                            { Rotate (strToAxis $2) $3 (Just $4) }
        | ROTATE STRING DOUBLE                                   { Rotate (strToAxis $2) $3 Nothing }
        | BASENAME STRING                                        { Basename $2 }
        | SAVE_KNOBS STRING                                      { SaveKnobs $2 }
        | TWEEN DOUBLE DOUBLE STRING STRING                      { Tween $2 $3 $4 $5 }
        | FRAMES DOUBLE                                          { Frames $2 }
        | VARY STRING DOUBLE DOUBLE DOUBLE DOUBLE                { Vary $2 $3 $4 $5 $6 }
        | PUSH                                                   { Push }
        | GENERATE_RAYFILES                                      { GenerateRayfiles }
        | POP                                                    { Pop }
        | SAVE STRING                                            { Save $2 }
        | SHADING SHADING_TYPE                                   { Shading (strToShadingType $2) }
        | SETKNOBS DOUBLE                                        { SetKnobs $2 }
        | FOCAL DOUBLE                                           { Focal $2 }
        | DISPLAY                                                { Display }
        | WEB                                                    { Web }
        | AMBIENT DOUBLE DOUBLE DOUBLE                           { Ambient (Triple $2 $3 $4) }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
