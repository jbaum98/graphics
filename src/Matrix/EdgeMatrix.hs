{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

module Matrix.EdgeMatrix (
    EdgeMatrix,
    -- ** Point Representation
    module Matrix.D3Point,
    -- ** Construction
    empty,
    fromPoints,
    connectPoints,
    point,
    edge,
    addEdge,
    (++),
    -- ** Retrieving 'Point's
    toPoints,
    toPointPairs,
    drawLinesColor,
    drawLines,
    ) where

import           Matrix.Base
import           Matrix.D3Point
import           D2Point
import           Pair
import           Color
import           Picture (Picture, drawColorLine)
import           Utils
import           Data.Array.Repa hiding ((++))
import qualified Data.Array.Repa  as R
import           Control.Monad.ST
import           Prelude hiding ((++))

-- | An 'EdgeMatrix' is a 'Matrix' that stores points in 3D space as columns. To
-- make transformations simpler, it uses homogenous coordinates: a point (x,y,z)
-- is stored in the matrix as a column (x,y,z,1). In addition, consecutive pairs
-- of points are interpreted as endpoints of line segments. An 'EdgeMatrix' is
-- implemented using a 'Matrix' of 'D3Coord's that is defferred. Functions that
-- consume 'EdgeMatrix's evaluate them so all the calulation happens once.
type EdgeMatrix = Matrix D D3Coord

instance Show EdgeMatrix where
  show = prettyPrint

empty :: EdgeMatrix
empty = delay $ fromListUnboxed (ix2 4 0) []

-- |Converts a list of 'D3Point's to an 'EdgeMatrix' containg them. It does not
-- check that the length of the list is even.
fromPoints :: [D3Point] -> EdgeMatrix
fromPoints points = fromFunction (ix2 4 len) f
  where
    f (Z :. r :. c) = pointToMatF r (points !! c)
    len = length points

-- |Converts a list of 'D3Points' to an 'EdgeMatrix' containing edges connecting
-- them: every point appears twice as the ending point of one segment and as the
-- starting point of the next segment.
connectPoints :: [D3Point] -> EdgeMatrix
connectPoints points = fromPoints . merge points $ tail . cycle $ points
  where
    merge _ [] = []
    merge [] _ = []
    merge (x:xs) (y:ys) = x : y : merge xs ys

-- |Given a 'D3Point' and the row of a matrix, extracts the correct coordinate.
pointToMatF :: Int -> D3Point -> D3Coord
pointToMatF 0 (Triple x _ _) = x
pointToMatF 1 (Triple _ y _) = y
pointToMatF 2 (Triple _ _ z) = z
pointToMatF 3 Triple {}      = 1
pointToMatF _ Triple {}      = 0

-- |Converts a single D3Point to an 'EdgeMatrix'
point :: D3Point -> EdgeMatrix
point p = fromFunction (ix2 4 1) f
  where
    f (Z :. r :. _) = pointToMatF r p

-- |Adds a single D3Point to an 'EdgeMatrix'
addPoint :: D3Point -> EdgeMatrix -> EdgeMatrix
addPoint = flip (++) . point

edge :: D3Point -> D3Point -> EdgeMatrix
edge p1 p2 = point p1 ++ point p2

-- |Adds an edge conecting to 'D3Point's to an 'EdgeMatrix'
addEdge :: D3Point -> D3Point -> EdgeMatrix -> EdgeMatrix
addEdge p1 p2 m = addPoint p2 $ addPoint p1 m

-- |Combine two 'EdgeMatrix's
(++) :: EdgeMatrix -> EdgeMatrix -> EdgeMatrix
(++) = (R.++)

-- |Gets the @n@th point from an 'EdgeMatrix' as a 'D2Point', dropping the
-- z-coordinate
getPoint :: Source r D3Coord => Array r DIM2 D3Coord -> Int -> D2Point
getPoint m n = pointFromList . fmap round . toList $ slice m (Any :. n)
  where
    pointFromList (x:y:_) = Pair x y

-- |Converts an 'EdgeMatrix' to a list of 'D2Point's, dropping their
-- z-coordinates. Evaluates the 'EdgeMatrix' in parallel.
toPoints :: EdgeMatrix -> [D2Point]
toPoints m = fmap (getPoint compM)  [0 .. len - 1]
  where
    compM = runST $ computeUnboxedP m
    Z :. _ :. len = extent m

-- |Converts an 'EdgeMatrix' to a list of 'Pair's of 'D2Point's, dropping their
-- z-coordinates. Evaluates the 'EdgeMatrix' in parallel.
toPointPairs :: EdgeMatrix -> [Pair D2Point]
toPointPairs m = [ Pair (getPoint compM n) (getPoint compM (n + 1))
                 | n <- [0,2 .. len - 1] ]
  where
    Z :. _ :. len = extent m
    compM = runST $ computeUnboxedP m

-- |Draws all lines specified by an 'EdgeMatrix' in a 'Color'
drawLinesColor :: Color -> EdgeMatrix -> Picture -> Picture
drawLinesColor color =
  compose . fmap (uncurryPair $ drawColorLine color) . toPointPairs

-- |Draws all lines specified by an 'EdgeMatrix' in a white
drawLines :: EdgeMatrix -> Picture -> Picture
drawLines = drawLinesColor white
