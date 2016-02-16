import Pbm

main :: IO ()
main = writePbm "pic.pbm" pic
  where f1 p = round (exp $ xgen * ygen :: Float)
          where Pair xgen ygen = fmap fromIntegral p
        f2 (Pair x y) = x*y
        f3 p = round ((sin xgen :: Float) + sin ygen + 1) * 75
           where Pair xgen ygen = fmap fromIntegral p
        pic = mathPic (Triple f1 f2 f3) (Pair 500 500)
