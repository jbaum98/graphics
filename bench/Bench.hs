import           Criterion.Main
import           Point
import           Pbm (writePbm)
import           Picture (blankPic)
import           System.IO.Temp
import           Control.DeepSeq

sizes :: [Coord]
sizes = [100, 500, 1000]

benchIO :: NFData a
        => (Point -> IO a) -> Coord -> Benchmark
benchIO f size = bench (showPair point) . nfIO . f $ point
  where
    point = pure size

benchPure :: NFData a
          => (Point -> a) -> Coord -> Benchmark
benchPure f size = bench (showPair point) . nf f $ point
  where
    point = pure size

main :: IO ()
main =
  defaultMain
    [ bgroup "create blank picture" $ map (benchPure blankPic) sizes
    , bgroup "writing tmp" $ map (benchIO writeBlank) sizes
    ]
  where
    writeBlank =
      withTempFile "." "graphics-benchmark.ppm" .
      const . writePbm . blankPic

showPair :: Show a
         => Pair a -> String
showPair (Pair x y) = show x ++ "x" ++ show y
