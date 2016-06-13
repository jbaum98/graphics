{-# LANGUAGE LambdaCase #-}

module Data.Picture.Drawing.Lighting (
  Lighting(..),
  PointLight(..),
  LightingConsts,
  ShadingType(..),
  defaultLightingConsts,
  calcLighting,
  vertexNormals
) where

import Data.Map hiding (foldl)
import Data.Monoid

import Data.Color
import Data.Pair
import {-# SOURCE #-} Data.Matrix.PolyMatrix (PolyMatrix(..))
import Data.Matrix.Base

data Lighting = Lighting
  { ambient :: !Color
  , lights  :: ![PointLight] }

data PointLight = PointLight
  { color :: !Color
  , loc   :: !(Triple Double) }

type LightingConsts = Triple (Triple Double)

defaultLightingConsts :: LightingConsts
defaultLightingConsts = pure . pure $ 0.3

calcLighting :: (Lighting, LightingConsts) -> Triple Double -> Triple Double -> Color
calcLighting (Lighting amb pointLights, Triple kr kg kb) n v =
  (trunc <$> iamb) + (trunc <$> idiff) + (trunc <$> ispec)
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

trunc :: Double -> ColorVal
trunc = truncate . boundColor

boundColor :: Double -> Double
boundColor n | n < 0 = 0
boundColor n | n > 255 = 255
boundColor n = n

mapSum :: Num b => [a] -> (a -> b) -> b
mapSum xs f = getSum $ foldMap (Sum . f) xs

pointLighting :: [PointLight] -> Triple Double -> Triple Double -> Pair (Triple Double)
pointLighting pointLights n' v' = mapSum pointLights $ \(PointLight c l) ->
  let l' = normalize l
      c' = fromIntegral <$> c
  in (* c') <$> Pair (diff l' n') (spec l' n' v')

diff :: Triple Double -> Triple Double -> Triple Double
diff l' n' = pure (l' `dot` n')

spec :: Triple Double -> Triple Double -> Triple Double -> Triple Double
spec l' n' v' = pure $ (r `dot` v') ^ k
  where
    r = (pure (2 * l' `dot` n') * n') - l'
    k = 7 :: Int

vertexNormals :: PolyMatrix -> Map (Triple Double) (Triple Double)
vertexNormals (PolyMatrix m) = Data.Map.map normalize $
  foldl addNormals mempty [ (p1,p2,p3)
                          | i <- [0,3.. cols m - 2],
                            let p1 = getD3Point m i
                                p2 = getD3Point m $ i + 1
                                p3 = getD3Point m $ i + 2
                          ]

addNormals :: Map (Triple Double) (Triple Double) -> (Triple Double, Triple Double, Triple Double) -> Map (Triple Double) (Triple Double)
addNormals hashmap (p1,p2,p3) = addNormal p1 . addNormal p2 . addNormal p3 $ hashmap
  where
    addNormal     = alter $ \case
      Nothing     -> Just n
      (Just oldN) -> Just $ n + oldN
    n = p2 - p1 `cross` p3 - p1

data ShadingType = Phong
                 | Flat
                 | Goroud
                 | Raytrace
                 | Wireframe
                 deriving (Eq, Show)
