module Picture (
  Picture,
  apF, size, blankPic, mathPic,
  setPixel,
  module X,
  fromList
  ) where

import Point as X
import Pixel as X
import Prelude hiding (length, head, replicate)
import Data.Sequence

type Picture = Seq (Seq Pixel)
type PixelFunc = Point -> ColorVal

apF :: Functor f => f (a -> b) -> a -> f b
apF fs val = fmap ($val) fs

size :: Picture -> Point
size = apF (Pair xres yres)
  where xres = fromIntegral . length . head
        yres = fromIntegral . length
        head = flip index 1

blankPic :: Point -> Picture
blankPic (Pair xr yr) = replicate (fromInteger yr + 1) oneRow
  where oneRow = replicate (fromInteger xr + 1) white

mathPic :: Triple PixelFunc -> Point -> Picture
mathPic funcs (Pair xr yr) = fromList [fromList [ genPixel (Pair x y) | x <- [0..xr]] | y <- [0..yr] ]
  where genPixel = apF cappedFuncs
        cappedFuncs = fmap ((`mod` maxPixel).) funcs

setPixel :: Pixel -> Point -> Picture -> Picture
setPixel pixel (Pair x y) pic = update (fromInteger y) newRow pic
  where newRow = update (fromInteger x) pixel oldRow
        oldRow = index pic $ fromInteger y
