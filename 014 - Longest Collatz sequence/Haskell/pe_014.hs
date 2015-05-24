{-
file:   pe_014.hs
title:  Longest Collatz sequence
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 23, 2015

link:   https://projecteuler.net/problem=14

note:   This solution is based on the fourth solution to Problem 14
    posted by user Muhkuh at wiki.haskell.org.
    <https://wiki.haskell.org/Euler_problems/11_to_20#Problem_14>
    <https://wiki.haskell.org/User:Muhkuh>
-}

import Data.Array
import Data.List  (maximumBy)
import Data.Ord   (comparing)

collatzLengths :: Int -> Array Int Int
collatzLengths max = collatzLengths'
  where
    collatzLengths' = listArray (0, max) $ 0:map collatzLength [1..]
    collatzLength n
        | n == 1    = 0
        | n' <= max = 1 + collatzLengths' ! n'
        | otherwise = 1 + collatzLength n'
      where
        n' | even n = quot n 2
           | odd n  = 3*n + 1

maxIndex :: Array Int Int -> Int
maxIndex = fst . maximumBy (comparing snd) . assocs

pe_014 = maxIndex $ collatzLengths 1000000

main :: IO ()
main = do
    print pe_014
