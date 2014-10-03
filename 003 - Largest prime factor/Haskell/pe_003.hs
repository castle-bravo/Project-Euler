{- |
file:   pe_003.hs
title:  Largest prime factor
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   September 20, 2014
-}

largestPrimeFactor :: Integer -> Integer -> Integer
largestPrimeFactor x y
    | x == y        = x
    | mod x y == 0  = largestPrimeFactor (quot x y) y
    | otherwise     = largestPrimeFactor x (y + 2)

pe_003 = largestPrimeFactor 600851475143 3

main :: IO ()
main = do
    print pe_003
