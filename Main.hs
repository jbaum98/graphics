import Pbm

main :: IO ()
main = writePbm "pic.pbm" pic
  where pic = drawLine (Pair 0 0) (Pair 100 70) $ blankPic (Pair 100 100)
