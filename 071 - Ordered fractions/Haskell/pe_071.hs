{-
file:   pe_071.hs
title:  Ordered fractions
author: Alexander Gosselin
e-mail: alexandergosselin@gmail.com
        alexander.gosselin@alumni.ubc.ca
date:   February 5, 2016

link:   <https://projecteuler.net/problem=144>

copyright:  (C) 2016 Alexander Gosselin
license:    GNU General Public License v3.0
            <http://www.gnu.org/licenses/gpl-3.0.en.html>
-}

import Data.Maybe

-- extended Euclidean algorithm
-- <https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm>
gcdExtended :: (Ord a, Integral a) => 
               a -> a -> ((a, a), (a, a), (a, a))
gcdExtended a b = gcdExtended' ((a, b), (1, 0), (0, 1))
  where
    gcdExtended' ((r, 0),  (s, s'), (t, t')) = 
        ((r, 0), (s, s'), (t, t'))
    gcdExtended' ((r, r'), (s, s'), (t, t')) =
        gcdExtended' ((r', r''), (s', s''), (t', t'')) 
      where
        r'' = r - q*r'
        s'' = s - q*s'
        t'' = t - q*t'
        q   = quot r r'

-- modular multiplicative inverse
-- <https://en.wikipedia.org/wiki/Modular_multiplicative_inverse>
modInverse :: (Ord a, Integral a) => a -> a -> Maybe a
modInverse a m = modInverse' $ gcdExtended m a
  where
    modInverse' ((r, _), _, (t, _))
        | r > 1     = Nothing
        | t < 0     = Just (m + t)
        | otherwise = Just t

-- largest fraction n/d less than p/q subject to d <= dMax
-- derivation:
--    n/d < p/q
-- => nq  < pd
-- suppose we make this difference as small as possible
--    nq = pd - 1   
-- => pd := 1 mod q
-- d is a modular multiplicative inverse of p mod q, so d has the form
--    d = kq + a
-- where a is the smallest modular multiplicative inverse of p mod q, 
-- and k is an integer constant.
--    d <= dMax
-- => k = floor (dmax - a)/q
-- Now that we have d, we can easily compute n
--    n = pd/q
-- by Bezout's identity, if pd - nq = 1, then n and d are also coprime
-- so we do not need to divide by gcd(n, d) 
-- <https://en.wikipedia.org/wiki/B%C3%A9zout's_identity>
largestFractionLessThan :: (Integral a) => (a, a) -> a -> (a, a)
largestFractionLessThan (p, q) dMax = (n, d)
  where
    n = quot (p*d) q
    d = (k*q) + a
    k = quot (dMax - a) q
    a = fromJust $ modInverse p q

pe_071 = fst $ largestFractionLessThan (3, 7) 1000000

main :: IO ()
main = do
    print pe_071

