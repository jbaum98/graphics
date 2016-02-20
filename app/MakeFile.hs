module MakeFile (makeFile) where

import Pbm
import Picture

makeFile :: String -> IO ()
makeFile path = writePbm path pic
  where f1 (Pair x y) = round $ exp (fromIntegral x * fromIntegral y)
        f2 (Pair x y) = x*y
        f3 (Pair x y) = round $ (sini x + sini y + 1) * 75
          where sini = sin . fromIntegral
        pic = mathPic (Triple f1 f2 f3) (Pair 500 500)
