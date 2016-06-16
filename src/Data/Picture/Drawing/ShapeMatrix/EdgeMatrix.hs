{-# LANGUAGE BangPatterns #-}

module Data.Picture.Drawing.ShapeMatrix.EdgeMatrix (
    EdgeMatrix,
    addEdge
    ) where

import Control.Monad

import Data.Pair
import Data.Matrix
import Data.Picture.Drawing.ShapeMatrix.ShapeMatrix
import Data.Picture.Drawing.Line

newtype EdgeMatrix = EdgeMatrix { runEM :: Matrix Double }

instance ShapeMatrix EdgeMatrix where
  drawColor color _ _ (EdgeMatrix m) = forM_ [Pair (get3DPoint m i) (get3DPoint m (i+1)) | i <- [0,2.. cols m - 1]] . \pic (Pair (Triple !x1 !y1 !z1) (Triple !x2 !y2 !z2)) -> drawColorLine color (round x1) (round y1) z1 (round x2) (round y2) z2 pic
  unwrap = runEM
  wrap = EdgeMatrix

instance Monoid EdgeMatrix where
  mempty = wrap empty
  mappend = liftDraw2 mergeCols

addEdge :: Triple Double -> Triple Double -> EdgeMatrix -> EdgeMatrix
addEdge p1 p2 = wrap . addP p2 . addP p1 . unwrap
