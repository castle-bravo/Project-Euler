{- |
file:   pe_007.hs
title:  10001st Prime
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 23, 2015

link:   https://projecteuler.net/problem=7

references:
  O'Neill, Melissa E. "The Genuine Sieve of Eratosthenes." 
  J. Funct. Prog. Journal of Functional Programming (2008).
  Web. 23 May 2015. 
  <https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>

  "Sieve of Eratoesthenes." Wikipedia. Wikimedia Foundation. Web. 23 May 2015.
  <http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes>

copyright:  (c) 2008 Richard Bird
            Used without permission
            From "The Genuine Sieve of Eratosthenes", page 11
-}

primes = 2:subtract' [3..] composites
    where
        composites = union [multiples p | p <- primes]

multiples n = map (n*) [n..]

subtract' (x:xs) (y:ys) 
    | x < y   = x:subtract' xs (y:ys)
    | x == y  = subtract' xs ys
    | x > y   = subtract' (x:xs) ys

union = foldr merge []
    where
        merge (x:xs) ys = x:merge' xs ys
        merge' (x:xs) (y:ys)
            | x < y   = x:merge' xs (y:ys)
            | x == y  = x:merge' xs ys
            | x > y   = y:merge' (x:xs) ys

pe_007 = last $ take 10001 primes

main :: IO ()
main = do
    print pe_007
