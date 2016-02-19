module Picture (
  Picture,
  apF, size, blankPic, mathPic,
  setPixel, setPixels, setColor, setOrigin,
  module X,
  fromList
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Sequence       hiding (zip)
import           Pixel               as X
import           Point               as X
import           Prelude             hiding (head, length, replicate)

type Picture = Seq (Seq Pixel)
type PixelFunc = Point -> ColorVal

apF :: Functor f => f (a -> b) -> a -> f b
apF fs val = fmap ($val) fs

size :: Picture -> Point
size = apF (Pair xres yres)
  where xres = length . head
        yres = length
        head = flip index 1

blankPic :: Point -> Picture
blankPic (Pair xr yr) = replicate (yr + 1) oneRow
  where oneRow = replicate (xr + 1) white

mathPic :: Triple PixelFunc -> Point -> Picture
mathPic funcs (Pair xr yr) = fromList [fromList [ genPixel (Pair x y) | x <- [0..xr]] | y <- [0..yr] ]
  where genPixel = apF cappedFuncs
        cappedFuncs = fmap ((`mod` maxPixel).) funcs

setPixel :: Pixel -> Point -> Picture -> Picture
setPixel pixel (Pair x y) pic = update y newRow pic
  where newRow = update x pixel oldRow
        oldRow = index pic y

setPixels :: [Pixel] -> [Point] -> Picture -> Picture
setPixels pixels points = foldl (.) id setAllPixels
  where setAllPixels = map (uncurry setPixel) pairs
        pairs = zip pixels points

setColor :: Pixel -> [Point] -> Picture -> Picture
setColor color = setPixels (repeat color)

setOrigin :: Point -> (Point -> Point)
setOrigin o = translate o . reflect
  where reflect = (<*>) $ Pair id negate
