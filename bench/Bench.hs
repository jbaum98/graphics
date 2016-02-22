import Criterion.Main
import Pair
import GenerateFile (pic)
import Pbm (writePbm)
import Picture (blankPic)

main :: IO ()
main = defaultMain [
         bgroup "rendering lines" [
             bench "100x100"   $ whnf pic (Pair 100 100)
           , bench "500x500"   $ whnf pic (Pair 500 500)
           , bench "1000x1000" $ whnf pic (Pair 1000 1000)
             ]
         , bgroup "create blank picture" [
              bench "100x100"   $ whnf blankPic (Pair 100 100)
            , bench "500x500"   $ whnf blankPic (Pair 500 500)
            , bench "1000x1000" $ whnf blankPic (Pair 1000 1000)
             ]
         , bgroup "writing" [
               bench "100x100"   . nfIO . writePbm "bench.ppm" . blankPic $ Pair 100 100
             , bench "500x500"   . nfIO . writePbm "bench.ppm" . blankPic $ Pair 500 500
             , bench "1000x1000" . nfIO . writePbm "bench.ppm" . blankPic $ Pair 1000 1000
             ]
          ]
