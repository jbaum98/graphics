{-# LANGUAGE TypeOperators, FlexibleContexts, ConstraintKinds #-}

{-|
Module      : Pbm
Description : NetPBM format

Write a 'Picture' to a file in the NetPBM format.
-}
module Pbm (writePbmFile, writePbm, fileContents) where

import Picture
import Color (maxColor)
import Data.ByteString.Builder
import Data.Monoid
import Data.Foldable
import System.IO

-- |Write a 'Picture' to a 'FilePath' in the NetPBM format
writePbmFile :: Repr r => FilePath -> Picture r -> IO ()
writePbmFile path pic = withFile path WriteMode $ writePbm pic

-- |Write a 'Picture' to a 'Handle' in the NetPBM format
writePbm :: Repr r => Picture r -> Handle -> IO ()
writePbm pic h = do
  hSetBinaryMode h True
  hSetBuffering h $ BlockBuffering Nothing
  hPutBuilder h $ fileContents pic

-- |Produce the 'Builder' string representation of the file
-- for a 'Picture' including the header
fileContents :: Repr r => Picture r -> Builder
fileContents pic = foldMap (<> char7 ' ') pieces <> char7 '\n' <> pixels
  where pieces = [string7 "P3", xresStr, yresStr, intDec maxColor]
        Pair xresStr yresStr = intDec <$> size pic
        pixels = renderPic pic

renderPic :: Repr r => Picture r -> Builder
{-# INLINE renderPic #-}
renderPic pic = fold [renderRow r | r <- [0..rows]]
  where Pair rows cols = size pic
        {-# INLINE renderRow #-}
        renderRow r = fold [renderPixel r c | c <- [0..cols]]
        {-# INLINE renderPixel #-}
        renderPixel x y = renderColor color
          where color = pic ! Pair x y
        renderColor = foldMap intDec
