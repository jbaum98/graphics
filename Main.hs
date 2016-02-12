import Pbm

main :: IO ()
main = writePbm "pic.pbm" pic
  where f1 (Pair x y) = round $ exp( (fromIntegral x) * (fromIntegral y))
        f2 (Pair x y) = x*y
        f3 (Pair x y) = round $ (sin (fromIntegral x) + sin (fromIntegral y) + 1) * 75
        pic = mathPic (Triple f1 f2 f3) (Pair 500 500)
