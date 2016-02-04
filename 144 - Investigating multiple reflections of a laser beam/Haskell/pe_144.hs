{-
file:   pe_144.hs
title:  Investigating multiple reflections of a laser beam
author: Alexander Gosselin
e-mail: alexandergosselin@gmail.com
        alexander.gosselin@alumni.ubc.ca
date:   February 4, 2016

link:   <https://projecteuler.net/problem=144>

copyright:  (C) 2016 Alexander Gosselin
license:    GNU General Public License 
-}

import Data.Complex

type R = Double
type C = Complex R
data Ellipse = Ellipse R R -- (x/a)^2 + (y/b)^2 = 1

normalize :: C -> C
normalize a = a/(magnitude a :+ 0)

-- reflection of vector z in tangent line t
tangentReflection :: C -> C -> C
tangentReflection z t = z*(cis (2*(phase t - phase z)))

-- reflection of vector z in normal line n
normalReflection :: C -> C -> C -- not used here
normalReflection z n = z*(cis (2*(phase z - phase n)))

-- path length s along a vector z from point c on an ellipse to another
-- point c' on the ellipse
-- derivation:
-- c' = z*s + c
-- ci' + cj' = (zi + zj)*s + (ci + cj)
-- (ci'/a)^2 + (cj'/b)^2 = 1                 (equation of an ellipse)
-- ((zx*s + cx)/a)^2 + ((zy*s + cy)/b)^2 = 1 (above applied to rhs)
-- ((zx/a)^2 + (zy/b)^2)s^2 + 2(zi*ci/a^2 + zj*cj/b^2)s
--    + (ci/a)^2 + (cj/b)^2 - 1 = 0
-- (ci/a)^2 + (cj/b)^2 - 1 = 0, since c is on the ellipse
-- => s = -2(zi*ci/a^2 + zj*cj/b^2)/((zx/a)^2 + (zy/b)^2)
ellipsePathLength :: Ellipse -> (C, C) -> R
ellipsePathLength (Ellipse a b) ((zi :+ zj), (ci :+ cj)) =
    -2*(zi*ci/a^^2 + zj*cj/b^^2)/((zi/a)^^2 + (zj/b)^^2)

-- tangent vector of ellipse at point c (discontinuous)
ellipseTangent :: Ellipse -> C -> C
ellipseTangent (Ellipse a b) (x :+ y) = (-y/b^^2) :+ (x/a^^2)

-- outward oriented normal vector from an ellipse
ellipseNormal :: Ellipse -> C -> C -- not used here
ellipseNormal (Ellipse a b) (x :+ y) = (x/a^^2) :+ (y/b^^2)

tracePath :: Ellipse -> (C, C) -> (C, C)
tracePath e a@(z, c) = (tangentReflection z (ellipseTangent e c'), c')
  where
    c' = z*(ellipsePathLength e a :+ 0) + c

pe_144 = length
    $ takeWhile (\(_, x :+ y) -> not $ exit (x, y))
    $ iterate (tracePath whiteCell) a1
  where
    -- 4x^2 + y^2 = 100 is equivalent to (x/5)^2 + (y/10)^2 = 1
    whiteCell = Ellipse 5 10 
    exit = (\(x, y) -> y > 0 && x > -0.01 && x < 0.01)
    a1 = (tangentReflection z0 (ellipseTangent whiteCell c1), c1)
    z0 = normalize (c1 - c0)
    c0 = 0 :+ 10.1
    c1 = 1.4 :+ (-9.6)

main :: IO ()
main = do
    print pe_144
