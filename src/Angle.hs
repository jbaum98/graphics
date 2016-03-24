module Angle (
  degToRad, radToDeg
  ) where

degToRad :: (Num a, RealFrac a, Floating a) => a -> a
degToRad = (/ 180) . (* pi)

radToDeg :: (Num a, RealFrac a, Floating a) => a -> a
radToDeg = (* 180) . (/ pi)
