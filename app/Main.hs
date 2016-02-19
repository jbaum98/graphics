import Pbm

main :: IO ()
main = writePbm "pic.pbm" pic
  where maxPoint = Pair 1000 1000
        origin = (round . (/2) . fromIntegral) <$> maxPoint
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
        pic = drawLines origin lineList $ blankPic maxPoint

drawLines :: Point -> [(Point, Point, Pixel)] -> Picture -> Picture
drawLines origin points = foldl (.) id $ map drawLine points
  where drawLine (p1, p2, color) = setColor color . fmap (setOrigin origin) $ line p1 p2
