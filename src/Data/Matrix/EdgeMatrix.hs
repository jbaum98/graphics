{-# LANGUAGE BangPatterns #-}

module Data.Matrix.EdgeMatrix (
    EdgeMatrix,
    edge,
    addEdge
    ) where

import Control.Monad
import qualified Data.Vector.Unboxed as V

import Data.Pair
import Data.Matrix.Base
import Data.Matrix.ShapeMatrix
import Data.Picture.Drawing.Line

newtype EdgeMatrix = EdgeMatrix { runEM :: Matrix Double }

instance ShapeMatrix EdgeMatrix where
  drawColor color _ _ (EdgeMatrix m) = forM_ [Pair (getD3Point m i) (getD3Point m (i+1)) | i <- [0,2.. cols m - 1]] . \pic (Pair (Triple !x1 !y1 !z1) (Triple !x2 !y2 !z2)) -> drawColorLine color (round x1) (round y1) z1 (round x2) (round y2) z2 pic
  unwrap = runEM
  wrap = EdgeMatrix

instance Monoid EdgeMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

edge :: Triple Double -> Triple Double -> EdgeMatrix
edge (Triple x1 y1 z1) (Triple x2 y2 z2) = EdgeMatrix $ Matrix 4 2 7 $
  V.fromList [x1, y1, z1, 1, x2, y2, z2, 1]

addEdge :: Triple Double -> Triple Double -> EdgeMatrix -> EdgeMatrix
addEdge p1 p2 = wrap . addP p2 . addP p1 . unwrap
