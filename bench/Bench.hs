import Criterion.Main
import Pair
import GenerateFile (pic)
import Pbm (writePbm)
import Picture (blankPic)

main :: IO ()
main = defaultMain [
  bgroup "generating picture" [
      bench "100x100"   . nf pic $ Pair 100 100
    , bench "500x500"   . nf pic $ Pair 500 500
    , bench "1000x1000" . nf pic $ Pair 1000 1000
    ]
  , bgroup "create blank picture" [
      bench "100x100"   . nf blankPic $ Pair 100 100
    , bench "500x500"   . nf blankPic $ Pair 500 500
    , bench "1000x1000" . nf blankPic $ Pair 1000 1000
      ]
  , bgroup "writing" [
      bench "100x100"   . nfIO . writePbm "bench.ppm" . blankPic $ Pair 100 100
     , bench "500x500"   . nfIO . writePbm "bench.ppm" . blankPic $ Pair 500 500
     , bench "1000x1000" . nfIO . writePbm "bench.ppm" . blankPic $ Pair 1000 1000
     ]
  ]
