module Pbm (
  writePbm,
  blankPic,
  mathPic,
  Picture, Pixel, Coord,
  Pair(..), Triple(..)
  )
where

import Pair

maxPixel :: ColorVal
maxPixel = 256

type ColorVal = Integer
data Pixel = Pixel { red :: ColorVal, green :: ColorVal, blue :: ColorVal }
type Picture = [[Pixel]]
type Coord = Pair Integer
type PixelFunc = Coord -> ColorVal

apF :: Functor f => f (a -> b) -> a -> f b
apF fs val = fmap ($val) fs

size :: Picture -> Coord
size = apF (Pair xres yres)
  where xres = fromIntegral . length . head
        yres = fromIntegral . length

writePbm :: FilePath -> Picture -> IO ()
writePbm path pic = foldl (>>) clearFile $ map (appendFileLine path) pieces
  where pieces = ["P3", xresStr, yresStr, show maxPixel, pixels]
        Pair xresStr yresStr = fmap show $ size pic
        pixels = pixelsStr pic
        appendFileLine file s = (appendFile file s) >> (appendFile file "\n")
        clearFile = writeFile path ""

pixelsStr :: Picture -> String
pixelsStr = unlines . map rowStr
  where rowStr = unwords . map pixelStr

pixelStr :: Pixel -> String
pixelStr = unwords . map show . colorList
  where colorList =  apF [red, green, blue]

blankPic :: Coord -> Picture
blankPic (Pair xr yr) = take (fromInteger yr) $ repeat oneRow
  where oneRow = take (fromInteger xr) $ repeat (Pixel 0 0 0)

mathPic :: Triple PixelFunc -> Coord -> Picture
mathPic funcs (Pair xr yr) =
  [[ genPixel (Pair x y) | x <- [0..xr]] | y <- [0..yr]]
  where genPixel = uncurryTriple Pixel . apF cappedFuncs
        cappedFuncs = fmap ((`mod` maxPixel).) funcs
