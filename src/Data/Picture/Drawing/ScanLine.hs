{-# LANGUAGE ViewPatterns, ParallelListComp #-}

module Data.Picture.Drawing.ScanLine (
  scanLine, sortPoints
  ) where

import Control.Monad
import Control.Monad.ST

import Control.Loop

import Data.Color
import Data.D2Point (getY)
import Data.Pair
import Data.Picture.Drawing.Line (writeLine)
import Data.Picture.Drawing.Points (reflect)
import Data.Picture.Picture

scanLine :: Color -> Triple Double -> Triple Double -> Triple Double -> Picture -> Picture
scanLine color p1 p2 p3 pic = mutPic pic $ \mpic -> writeScanLine color b m t mpic >> return mpic
  where
    Triple t m b = sortPoints p1' p2' p3'
    p1' = standardize p1
    p2' = standardize p2
    p3' = standardize p3
    standardize = reflect yMax . dropZ
    Pair _ yMax = getSize pic

writeScanLine :: Color -> Pair Double -> Pair Double -> Pair Double -> MPicture s -> ST s ()
writeScanLine color bot@(Pair xb yb) mid@(Pair xm ym) top@(getY -> yt) mArr = do
  Pair l _ <- forLoopState yb (< ym) (+1) (pure xb) $ \(Pair xl xr) y -> do
    writeLine (round <$> Pair xl y) (round <$> Pair xr y) color mArr
    return $ Pair (xl + dxL) (xr + dxR1)
  void $ forLoopState ym (< yt) (+1) (Pair l xm) $ \(Pair xl xr) y -> do
    writeLine (round <$> Pair xl y) (round <$> Pair xr y) color mArr
    return $ Pair (xl+dxL) (xr+dxR2)
  where
    dxL  = fSlope bot top
    dxR1 = fSlope bot mid
    dxR2 = fSlope mid top

fSlope :: Pair Double -> Pair Double -> Double
fSlope (Pair x0 y0)  (Pair x1 y1) = (x1 - x0) / (y1 - y0)

sortPoints :: Pair Double -> Pair Double -> Pair Double -> Triple (Pair Double)
sortPoints p1@(getY -> y1) p2@(getY -> y2) p3@(getY -> y3) | y1 >= y2 && y1 >= y3 && y2 >= y3 = Triple p1 p2 p3
sortPoints p1@(getY -> y1) p2@(getY -> y2) p3@(getY -> y3) | y1 >= y2 && y1 >= y3           = Triple p1 p3 p2

sortPoints p1@(getY -> y1) p2@(getY -> y2) p3@(getY -> y3) | y2 >= y1 && y2 >= y3 && y1 >= y3 = Triple p2 p1 p3
sortPoints p1@(getY -> y1) p2@(getY -> y2) p3@(getY -> y3) | y2 >= y1 && y2 >= y3           = Triple p2 p3 p1

sortPoints p1@(getY -> y1) p2@(getY -> y2) p3@(getY -> _)  |                     y1 >= y2 = Triple p3 p1 p2
sortPoints p1@(getY -> _)  p2@(getY -> _)  p3@(getY -> _)  | otherwise                   = Triple p3 p2 p1


dropZ :: Triple a -> Pair a
dropZ (Triple x y _) = Pair x y
