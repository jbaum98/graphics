module GenerateFile (generateFile, pic) where

import Picture hiding (drawLine)
import Matrix
import Pbm

generateFile :: String -> D2Point -> IO ()
generateFile path maxPair= writePbmFile path $ pic maxPair

pic :: D2Point -> Picture
pic = drawProgressColors transforms edges . blankPic

transforms :: [(TransformMatrix, Color)]
transforms = [
  (idMatrix, black),
  (scaleMatrix $ Triple 7 7 7, red),
  (rotZMatrix 30, blue),
  (transMatrix $ Triple 200 200 0, orange),
  (rotXMatrix 45, green),
  (transMatrix $ Triple (-500) 0 0, violet),
  (rotYMatrix 60, yellow)
  ]

edges :: EdgeMatrix
edges = fromPoints [
  Triple  (-10)  (-10)  10,  Triple  (-10)  10     10,
  Triple  (-10)  10     10,  Triple  10     10     10,
  Triple  10     10     10,  Triple  10     (-10)  10,
  Triple  10     (-10)  10,  Triple  (-10)  (-10)  10,
  Triple  10     10     10,  Triple  10     10     (-10),
  Triple  10     (-10)  10,  Triple  10     (-10)  (-10),
  Triple  (-10)  10     10,  Triple  (-10)  10     (-10),
  Triple  (-10)  (-10)  10,  Triple  (-10)  (-10)  (-10),
  Triple  (-10)  (-10)  (-10),  Triple  (-10)  10     (-10),
  Triple  (-10)  10     (-10),  Triple  10     10     (-10),
  Triple  10     10     (-10),  Triple  10     (-10)  (-10),
  Triple  10     (-10)  (-10),  Triple  (-10)  (-10)  (-10)
  ]
