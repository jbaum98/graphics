import Pbm
import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = writePbm "pic.pbm" pic
  where maxPoint = Pair 100 100
        origin = (round . (/2) . fromIntegral) <$> maxPoint
        lineList = [
          Pair (Pair 0 0) (Pair 100 70),
          Pair (Pair 0 (-10)) (Pair 100 70)
          ]
        pic = drawLines origin lineList $ blankPic maxPoint

drawLines :: Point -> [Pair Point] -> Picture -> Picture
drawLines origin points = foldl (.) id $ drawAllLines points
   where drawAllLines = map $ setColor black . fmap (setOrigin origin) . uncurryPair line
