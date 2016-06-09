module Data.Picture.Drawing.Lighting (
  Lighting(..),
  PointLight(..),
  LightingConsts,
  calcLighting
) where

import Debug.Trace

import Data.Monoid
import Control.Applicative

import Data.Color
import Data.Pair

trace' x = trace (show x) x

data Lighting = Lighting
  { ambient :: !Color
  , lights  :: ![PointLight] }

data PointLight = PointLight
  { color :: !Color
  , loc   :: !(Triple Double) }

type LightingConsts = Triple (Triple Double)

calcLighting :: (Lighting, LightingConsts) -> Triple Double -> Triple Double -> Color
calcLighting (Lighting amb pointLights, Triple kr kg kb) n v =
  (trunc <$> iamb) + (trunc <$> idiff) + (trunc <$> ispec)
  where
    n' = normalize n
    v' = normalize v
    iamb  = kamb * (fromIntegral <$> amb)
    idiff = kdiff * diffLight
    ispec = kspec * specLight
    Pair diffLight specLight = trace' $ pointLighting pointLights n' v'

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
  let l' = negate $ normalize l
      c' = fromIntegral <$> c
  in (* c') <$> Pair (diff l' n') (spec l' n' v')

diff :: Triple Double -> Triple Double -> Triple Double
diff l' n' = pure ((-l') `dot` n')

spec :: Triple Double -> Triple Double -> Triple Double -> Triple Double
spec l' n' v' = pure $ (r `dot` v') ^ k
  where
    r = (pure (2 * l' `dot` n') * n') - l'
    k = 7 :: Int
