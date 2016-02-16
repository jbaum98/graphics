module Picture (
  Picture,
  apF, size, blankPic, mathPic,
  setPixel,
  module X
  ) where

import Point as X
import Pixel as X
import Prelude hiding (length, head, replicate)
import Data.Vector

type Picture = Vector (Vector Pixel)
type PixelFunc = Point -> ColorVal

apF :: Functor f => f (a -> b) -> a -> f b
apF fs val = fmap ($val) fs

size :: Picture -> Point
size = apF (Pair xres yres)
  where xres = fromIntegral . length . head
        yres = fromIntegral . length

blankPic :: Point -> Picture
blankPic (Pair xr yr) = replicate (fromInteger yr + 1) oneRow
  where oneRow = replicate (fromInteger xr + 1) white

mathPic :: Triple PixelFunc -> Point -> Picture
mathPic funcs (Pair xr yr) = generate (fromInteger yr + 1) oneRow
  where oneRow y = generate (fromInteger xr + 1) $ \x -> genPixel . fmap fromIntegral $ Pair x y
        genPixel = apF cappedFuncs
        cappedFuncs = fmap ((`mod` maxPixel).) funcs

setPixel :: Pixel -> Point -> Picture -> Picture
setPixel pixel (Pair x y) pic = (//) pic [(fromInteger y, newRow)]
  where newRow = (//) oldRow [(fromInteger x, pixel)]
        oldRow = pic ! fromInteger y
