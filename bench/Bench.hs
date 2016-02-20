import Criterion.Main
import MakeFile (makeFile)

main :: IO ()
main = defaultMain [
  bench "makeFile" $ nfIO (makeFile "bench.ppm")
  ]
