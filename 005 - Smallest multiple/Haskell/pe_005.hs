{- |
file:   pe_005.hs
title:  Smallest multiple
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   September 29, 2014
-}

smallestMultiple :: Integer -> Integer
smallestMultiple x =
  product $ map (maxPower x 1) primes
  where
    primes = 
      [ y | y <- [2..x], not (any (0==) (map (mod y) [2..(y - 1)]))]
    maxPower :: Integer -> Integer -> Integer -> Integer
    maxPower x y z
      | z^(y + 1) < x  = maxPower x (y + 1) z
      | otherwise      = z^y

pe_005 = smallestMultiple 20

main :: IO ()
main = do
    print pe_005
