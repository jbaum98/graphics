{-# LANGUAGE FlexibleContexts #-}

module EdgeMatrix (
  EdgeMatrix, Coord,
  -- * Construction
  empty, fromPoints, addEdge,
  -- * Retrieving 'Point's
  toPoints, toPointPairs, drawMatLinesColor, drawMatLines
  ) where

import           Matrix
import           Pair
import           Color
import           Picture (Picture, drawColorLine)
import           Utils
import           Data.Array.Repa
import qualified Line as L
import           Control.Monad.ST
import           Prelude hiding ((++))

type Coord = Double

type Point = Triple Coord

type EdgeMatrix = Matrix Coord

type D2Point = L.Point

fromPoints :: [Point] -> EdgeMatrix
fromPoints ps = fromFunction (ix2 4 len) f
  where
    f (Z :. r :. c) = pointToMatF r (ps !! c)
    len = length ps

pointToMatF :: Int -> Point -> Coord
pointToMatF 0 (Triple x _ _) = x
pointToMatF 1 (Triple _ y _) = y
pointToMatF 2 (Triple _ _ z) = z
pointToMatF 3 (Triple _ _ _) = 1
pointToMatF _ (Triple _ _ _) = 0

singlePointMat :: Point -> EdgeMatrix
singlePointMat p = fromFunction (ix2 4 1) f
  where
    f (Z :. r :. _) = pointToMatF r p

addPoint :: Point -> EdgeMatrix -> EdgeMatrix
addPoint = flip (++) . singlePointMat

addEdge :: Point -> Point -> EdgeMatrix -> EdgeMatrix
addEdge p1 p2 m = addPoint p2 $ addPoint p1 m

getPoint :: Source r Coord => Array r DIM2 Coord -> Int -> D2Point
getPoint s n = pointFromList . fmap round . toList . slice s $ (Any :. n)

pointFromList :: [a] -> Pair a
pointFromList (x:y:_) = Pair x y
pointFromList _ = error "Tried to convert a list without two elements to a Point"

toPoints :: EdgeMatrix -> [D2Point]
toPoints s = [ getPoint s n
             | n <- [0 .. len - 1] ]
  where
    Z :. _ :. len = extent s

toPointPairs :: EdgeMatrix -> [Pair D2Point]
toPointPairs m = [ Pair (getPoint compM n) (getPoint compM (n + 1))
                 | n <- [0,2 .. len - 1] ]
  where
    Z :. _ :. len = extent m
    compM = runST $ computeUnboxedP m

drawMatLinesColor :: Color -> EdgeMatrix -> Picture -> Picture
drawMatLinesColor color = compose . fmap (uncurryPair $ drawColorLine color) . toPointPairs

drawMatLines :: EdgeMatrix -> Picture -> Picture
drawMatLines = drawMatLinesColor black
