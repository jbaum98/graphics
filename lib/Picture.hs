module Picture (
  Picture,
  apF, size, blankPic, mathPic,
  module X
  ) where

import Line as X
import Pixel as X
import Prelude hiding (length, head, replicate)
import Data.Vector (Vector, length, head, replicate, generate)

type Picture = Vector (Vector Pixel)
type PixelFunc = Point -> ColorVal

apF :: Functor f => f (a -> b) -> a -> f b
apF fs val = fmap ($val) fs

size :: Picture -> Point
size = apF (Pair xres yres)
  where xres = fromIntegral . length . head
        yres = fromIntegral . length

blankPic :: Point -> Picture
blankPic (Pair xr yr) = replicate (fromInteger yr) oneRow
  where oneRow = replicate (fromInteger xr) (Triple 0 0 0)

mathPic :: Triple PixelFunc -> Point -> Picture
mathPic funcs (Pair xr yr) = generate (fromInteger yr) oneRow
  where oneRow y = generate (fromInteger xr) $ \x -> genPixel . fmap fromIntegral $ Pair x y
        genPixel = apF cappedFuncs
        cappedFuncs = fmap ((`mod` maxPixel).) funcs
