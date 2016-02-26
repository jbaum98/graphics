module GenerateFile (generateFile, pic) where

import Point
import Pbm
import Picture

generateFile :: String -> Point -> IO ()
generateFile path = writePbmFile path . pic

pic :: Point -> Picture
pic maxPoint = mathPic (Triple f1 f2 f3) maxPoint
   where f1 (Pair x y) = round $ exp (fromIntegral x * fromIntegral y)
         f2 (Pair x y) = x*y
         f3 (Pair x y) = round $ (sini x + sini y + 1) * 75
           where sini = sin . fromIntegral
