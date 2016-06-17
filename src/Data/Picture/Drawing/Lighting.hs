{-# LANGUAGE LambdaCase, BangPatterns #-}

-- | For clarity, we use the following type synonyms:
--
-- > type Point  = Triple Double
-- > type Vector = Triple Double
--
-- but they are not exported because they would conflict with other modules.

module Data.Picture.Drawing.Lighting (
  Lighting(..),
  PointLight(..),
  LightingConsts,
  defaultLightingConsts,
  calcLighting,
  -- * Vertex Normals
  VertexNormalMap,
  vertexNormals,
  -- * Shading
  ShadingType(..),
) where

import qualified Data.Map as M
import Data.Monoid

import Data.Color
import Data.Pair
import {-# SOURCE #-} Data.Picture.Drawing.ShapeMatrix.PolyMatrix (PolyMatrix(..))
import Data.Matrix

type Point  = Triple Double
type Vector = Triple Double

-- | Represents the the lighting information for a frame
data Lighting = Lighting
  { ambient :: !Color
  , lights  :: ![PointLight] }

-- | Represents a single point light source
data PointLight = PointLight
  { color :: !Color
  , loc   :: !Point }

-- | Represents the lighting constants of a material as @Triple kr kg kb@ where
-- @kr@, @kg@, and @kb@ are each a @Triple ka kd ks@ representing the constants
-- for ambient, diffuse, and specular light, each for their respective colors
type LightingConsts = Triple (Triple Double)

data ShadingType = Phong
                 | Flat
                 | Goroud
                 | Raytrace
                 | Wireframe
                 deriving (Eq, Show)

-- | A map between 'Point's and their vertex normal 'Vector'
type VertexNormalMap = M.Map Point Vector

-- | By default, all lighting constants are set to 0.3
defaultLightingConsts :: LightingConsts
defaultLightingConsts = pure . pure $ 0.3

-- | Calculate the lighting based on a normal vector
calcLighting :: (Lighting, LightingConsts)
             -> Vector -- ^ normal vector
             -> Vector -- ^ view vector
             -> PreciseColor
calcLighting (Lighting amb pointLights, Triple kr kg kb) n v =
  (boundColor <$> iamb) + (boundColor <$> idiff) + (boundColor <$> ispec)
  where
    n' = normalize n
    v' = normalize v
    iamb  = kamb * (fromIntegral <$> amb)
    idiff = kdiff * diffLight
    ispec = kspec * specLight
    Pair diffLight specLight = pointLighting pointLights n' v'

    kamb = Triple kar kag kab
    kdiff = Triple kdr kdg kdb
    kspec = Triple ksr ksg ksb
    Triple kar kdr ksr = kr
    Triple kag kdg ksg = kg
    Triple kab kdb ksb = kb

-- | Ensure that all are color values are in a sensible range
boundColor :: Double -> Double
boundColor n | n < 0 = 0
boundColor n | n > 255 = 255
boundColor n = n

-- | Convert a this of things to a number and add them
mapSum :: Num b => [a] -> (a -> b) -> b
mapSum xs f = getSum $ foldMap (Sum . f) xs
{-# INLINE mapSum #-}

-- | Calculate the net diffuse and specular light due to a list of 'PointLight's
pointLighting :: [PointLight]
              -> Vector              -- ^ unit normal vector
              -> Vector              -- ^ unit view vector
              -> Pair (PreciseColor) -- ^ @Pair (diffuseLight) (specularLight)@
pointLighting pointLights n' v' = mapSum pointLights $ \(PointLight c l) ->
  let l' = normalize l
      c' = fromIntegral <$> c
  in (* c') <$> Pair (diff l' n') (spec l' n' v')

-- | Calculate the intensity factor of diffuse light
diff :: Vector -- ^ unit light vector of the 'PointLight'
     -> Vector -- ^ unit normal vector
     -> PreciseColor
diff l' n' = pure (l' `dot` n')

-- | Calculate the intensity factor of specular light
spec :: Vector -- ^ unit light vector of the 'PointLight'
     -> Vector -- ^ unit normal vector
     -> Vector -- ^ unit view vector
     -> PreciseColor
spec l' n' v' = pure $ (r `dot` v') ^ k
  where
    r = (pure (2 * l' `dot` n') * n') - l'
    k = 7 :: Int

-- | Calculate the vertex normals for all the vertices in a 'PolyMatrix'
vertexNormals :: PolyMatrix -> VertexNormalMap
vertexNormals (PolyMatrix m) = M.map normalize $
  foldl addNormals mempty [ Triple p1 p2 p3
                          | i <- [0,3.. cols m - 2],
                            let p1 = get3DPoint m i
                                p2 = get3DPoint m $ i + 1
                                p3 = get3DPoint m $ i + 2
                          ]

-- | Add the contribution of one triangle to a 'VertexNormalMap'
addNormals :: VertexNormalMap -> Triple Point -> VertexNormalMap
addNormals hashmap (Triple !p1 !p2 !p3) = addNormal p1 . addNormal p2 . addNormal p3 $ hashmap
  where
    addNormal     = M.alter $ \case
      Nothing     -> Just n
      (Just oldN) -> Just $ n + oldN
    n = p2 - p1 `cross` p3 - p1
{-# INLINE addNormals #-}
