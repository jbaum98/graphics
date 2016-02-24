module GenerateFile (generateFile, pic) where

import Picture
import Pbm
import Color
import Line
import Control.Monad (foldM)

generateFile :: String -> Point -> IO ()
generateFile path maxPoint= do
  p <- pic maxPoint
  writePbm path p

pic :: Point -> IO Picture
pic maxPoint = drawLines origin lineList $ blankPic maxPoint
  where origin = (round . (/2) . fromIntegral) <$> maxPoint
        lineList = [
          -- First - red
          (Pair 0 0,      Pair 1000 700,       red),
          (Pair 0 300,    Pair 1000 700,       red),
          -- Second - yellow
          (Pair 0 0,      Pair 700 1000,       yellow),
          (Pair 0 (-300), Pair 700 1000,       yellow),
          -- Third - orange
          (Pair 0 0,      Pair (-700) 1000,    orange),
          (Pair 0 (-300), Pair (-700) 1000,    orange),
          -- Fourth - green
          (Pair 0 0,      Pair (-1000) 700,    green),
          (Pair 0 300,    Pair (-1000) 700,    green),
          -- Fifth - blue
          (Pair 0 0,      Pair (-1000) (-700), blue),
          (Pair 0 (-300), Pair (-1000) (-700), blue),
          -- Sixth - indigo
          (Pair 0 0,      Pair (-700) (-1000), indigo),
          (Pair 0 300,    Pair (-700) (-1000), indigo),
          -- Seventh - violet
          (Pair 0 0,      Pair 700 (-1000),    yellow),
          (Pair 0 300,    Pair 700 (-1000),    yellow),
          -- Eighth - black
          (Pair 0 0,      Pair 1000 (-700),    black),
          (Pair 0 (-300), Pair 1000 (-700),    black),
          -- Axes
          (Pair 0 0, Pair 500 0,               pink),
          (Pair 0 0, Pair (-500) 0,            pink),
          (Pair 0 0, Pair 0 500,               pink),
          (Pair 0 0, Pair 0 (-500),            pink),
           -- y = ±x
          (Pair 0 0, Pair 500 500,             turqouise),
          (Pair 0 0, Pair (-500) 500,          turqouise),
          (Pair 0 0, Pair (-500) (-500),       turqouise),
          (Pair 0 0, Pair 500 (-500),          turqouise)
          ]

drawLines :: Point -> [(Point, Point, Color)] -> Picture -> IO Picture
drawLines origin points pic = foldM (flip drawLine) pic points
  where drawLine (p1, p2, color) = setColor color . fmap (transformOrigin origin) $ line p1 p2
