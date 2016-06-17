{-# LANGUAGE BangPatterns, FlexibleContexts, TupleSections #-}

module Data.Picture.Drawing.Shapes (
    parametric,
    circle,
    hermite,
    bezier,
    box,
    torus,
    sphere
  ) where

import Data.Monoid

import qualified Data.Vector.Unboxed as V

import Data.Picture.Drawing.ShapeMatrix
import Data.Matrix.Base
import Data.Matrix.Points
import Data.Matrix.Mult
import Data.Pair

type Coord = Double
type Point = Triple Coord

-- | Produces an 'EdgeMatrix' of lines approximating a parametric equation
-- @f(t) = \< x(t), y(t), z(t) \>@ as /t/ ranges from 0 to 1
parametric :: Int              -- ^ The number of steps to take between 0 and 1
           -> (Double -> Point) -- ^ The function @f(t)@
           -> EdgeMatrix
parametric steps f = addPoints $ wrap $ emptyWith (round $ 4 / step)
  where
    addPoints = appEndo $ foldMap Endo [ addEdge p p'
                                       | t <- [0,step..1]
                                       , let Pair p p' = f <$> Pair t (t + step)
                                       ]
    step = recip $ fromIntegral steps - 1

-- | Produces a 'PointMatrix' of sampling of a parametric equation of two
-- variables @f(t,p) = < x(t,p), y(t,p), z(t,p) >@ as both /t/ and /p/ range
-- from 0 to 1
parametric2 :: Int                       -- ^ The number of steps to take between
                                        -- 0 and 1 for both /t/ and /p/
            -> (Double -> Double -> Point) -- ^ The function @f(t,p)@
            -> PointMatrix
parametric2 steps f = addPoints $ wrap $ emptyWith (4 * steps * steps)
  where
    addPoints = appEndo $ foldMap Endo [ addPoint $ f i j
                        | j <- [0,step..1], i <- [0,step..1]]
    step = recip $ fromIntegral steps - 1

-- | Produces an 'EdgeMatrix' for a circle
circle :: Int   -- ^ The number of steps to take, affects how densely the
               -- triangles are tiled
       -> Point -- ^ The center of the circle
       -> Coord -- ^ The radius
       -> EdgeMatrix
circle steps (Triple cx cy cz) r =
  addEdge firstPoint lastPoint $ parametric steps f
  where
    f t = Triple (x t) (y t) cz
    x t = cx + r * cos (2 * pi * t)
    y t = cy + r * sin (2 * pi * t)
    firstPoint = Triple (cx + r) cy cz
    lastPoint = f $ 1.0 - step
    step = recip $ fromIntegral steps - 1

-- | Produces an 'EdgeMatrix' for a cubic Hermite curve
hermite :: Int   -- ^ The number of steps to take, affects how densely the
                -- triangles are tiled
        -> Point -- ^ The starting point
        -> Point -- ^ The starting rate of change, given as a @Triple (dx\/dt)
                -- (dy\/dt) (dz\/dt)@
        -> Point -- ^ The ending point
        -> Point -- ^ The ending rate of range
        -> EdgeMatrix
hermite steps p0 r0 p1 r1 = matCurve hermMat steps p0 p1 r0 r1
  where hermMat = fromLists [
          [  2, -2,  1,  1 ],
          [ -3,  3, -2, -1 ],
          [  0,  0,  1,  0 ],
          [  1,  0,  0,  0 ]
          ]

-- | Produces an 'EdgeMatrix' for a cubic Bezier curve
bezier :: Int   -- ^ The number of steps to take, affects how densely the
               -- triangles are tiled
       -> Point -- ^ The starting point
       -> Point -- ^ The first control point
       -> Point -- ^ The second control point
       -> Point -- ^ The ending point
       -> EdgeMatrix
bezier = matCurve bezMat
  where bezMat = fromLists [
          [ -1,  3, -3, 1 ],
          [  3, -6,  3, 0 ],
          [ -3,  3,  0, 0 ],
          [  1,  0,  0, 0 ]
          ]

-- | Produces an 'EdgeMatrix' for a curve defined by some 'Matrix' and 4 control
-- points; a generalization of both the Hermite and Bezier curves
matCurve :: Matrix Coord -> Int -> Point -> Point -> Point -> Point -> EdgeMatrix
matCurve cMat steps p1 p2 p3 p4  = parametric steps $ \t ->
  let t' = pure t
  in t' * (t' * (t' * a + b) + c) + d
  where
    a = Triple (m ! (1,1)) (m ! (1,2)) (m ! (1,3))
    b = Triple (m ! (2,1)) (m ! (2,2)) (m ! (2,3))
    c = Triple (m ! (3,1)) (m ! (3,2)) (m ! (3,3))
    d = Triple (m ! (4,1)) (m ! (4,2)) (m ! (4,3))
    !m = cMat `matMult` pointMat
    pointMat = fromLists [
      explode p1,
      explode p2,
      explode p3,
      explode p4
      ]
    explode (Triple x y z) = [x, y, z]

-- | Produces a 'PolyMatrix' for a box
box :: Point -- ^ The top-left-front corner of the box
    -> Point -- ^ The size of the box, given as @Triple width height depth@
    -> PolyMatrix
box topLeft (Triple x y z) = addPolys $ wrap $ emptyWith 47
  where
    addPolys = appEndo $ foldMap Endo [
       -- Front
       addPoly topLeft botLeft topRight
     , addPoly botRight topRight botLeft
       -- Back
     , addPoly (topRight + d) (botRight + d) (topLeft + d)
     , addPoly (botLeft + d) (topLeft + d) (botRight + d)
       -- Left
     , addPoly (topLeft + d) (botLeft + d) topLeft
     , addPoly botLeft topLeft (botLeft + d)
       -- Right
     , addPoly topRight botRight (topRight + d)
     , addPoly (botRight + d) (topRight + d) botRight
       -- Top
     , addPoly (topLeft + d) topLeft (topRight + d)
     , addPoly topRight (topRight + d) topLeft
       -- Bottom
     , addPoly botLeft (botLeft + d) botRight
     , addPoly (botRight + d) botRight (botLeft + d)
     ]
    botLeft = topLeft - Triple 0 y 0
    botRight = botLeft + Triple x 0 0
    topRight = topLeft + Triple x 0 0
    d = Triple 0 0 (-z)

-- | Produces a 'PolyMatrix' for a torus
torus :: Point -- ^ The center
      -> Coord -- ^ The radius of the circles making up the torus
      -> Coord -- ^ The distance from the center of the torus of each center of
              -- the circles making up the torus
      -> Int   -- ^ The number of steps
      -> PolyMatrix
torus c r1 r2 steps = crissCross steps torusPoints True $ wrap $ emptyWith (6 * steps * steps)
  where
    torusPoints = genTorus c r1 r2 steps

-- | Generates the 'PointMatrix' of the points on a torus
genTorus :: Point -> Coord -> Coord -> Int -> PointMatrix
genTorus c r1 r2 steps = parametric2 steps f
  where
    f t p = Triple (x t p) (y t p) (z t p) + c
    x thetaT phiT = cos(phiT * 2 * pi) * (r1 * cos(thetaT * 2 * pi) + r2)
    y thetaT _ = r1 * sin (thetaT * 2 * pi)
    z thetaT phiT = sin(phiT * 2 * pi) * (r1 * cos(thetaT * 2 * pi) + r2)

-- | Produces a 'PolyMatrix' for a sphere
sphere :: Point -- ^ The center
       -> Coord -- ^ The radius
       -> Int   -- ^ The number of steps
       -> PolyMatrix
sphere c r steps = connectMiddles . connectCaps $ wrap $ emptyWith (6 * (steps - 2) * steps)
  where
    connectMiddles = crissCross steps spherePoints False
    connectCaps = appEndo $ foldMap Endo [connect (1, steps+1, 0) . connect (steps - 2, steps - 1, steps + steps - 2) | sliceN <- [0..steps - 2], let connect = connectSlice sliceN]
    spherePoints = genSphere c r steps
    connectSlice = connectShapeSlice (spherePoints, steps)

-- | Generates the 'PointMatrix' of the points on a sphere
genSphere :: Point -> Coord -> Int -> PointMatrix
genSphere c r steps = parametric2 steps f
  where
    f t p = Triple (x t p) (y t p) (z t p) + c
    x thetaT _ = r * cos (thetaT * pi)
    y thetaT phiT = r*sin(thetaT * pi) * cos(phiT * 2 * pi)
    z thetaT phiT = r*sin(thetaT * pi) * sin(phiT * 2 * pi)

-- | Given a 'PointMatrix', add the polygons connecting the points in a
-- criss-cross way common to a torus and the part of sphere that is not the
-- poles
crissCross :: Int  -- ^ The number of steps used to generate the 'PointMatrix'
           -> PointMatrix
           -> Bool -- ^ Whether or not to start from the first point, and is the
                  -- only difference between torus and sphere
           -> PolyMatrix -> PolyMatrix
crissCross steps points startFromTop = appEndo $ foldMap Endo [
  connect (i+1, i', i) .
  connect (i', i+1, i'+1)
  | i      <- [start..steps-2],
    sliceN <- [0..steps-2],
    let connect = connectSlice sliceN
        i'      = i + steps
  ]
  where start = if startFromTop then 0 else 1
        connectSlice = connectShapeSlice (points, steps)

-- | Connect 3 points of a single slice of a criss-cross, as in either a "wedge"
-- of a sphere or one ring of a torus
connectShapeSlice :: (PointMatrix, Int) -- ^ The 'PointMatrix' and the number of
                                       -- steps used to generate it: @(points,
                                       -- steps)@
                  -> Int                -- ^ The index of the specific slice to
                                       -- connect
                  -> (Int, Int, Int)    -- ^ The index within the slice of the 3
                                       -- points to connect
                  -> PolyMatrix -> PolyMatrix
connectShapeSlice (points,steps) sliceN (i1, i2, i3) = addPoly (getPoint i1) (getPoint i2) (getPoint i3)
    where
      getPoint n = V.unsafeIndex (slice sliceN) . (+ (n * 4)) <$> Triple 0 1 2
      slice = circSlice $ unwrap points
      circSlice m i = V.slice (steps * i * 4) (4*steps) (vector m)
