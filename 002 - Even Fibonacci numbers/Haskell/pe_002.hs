{- |
file:   pe_002.hs
title:  Even Fibonacci numbers
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   September 21, 2014
-}

evenFibonacci = 
    0 : 2 : zipWith (+) 
        (evenFibonacci) 
        (map (* 4) (tail evenFibonacci))

pe_002 = 
    sum [ x | x <- takeWhile (< 4000000) evenFibonacci]

main :: IO ()
main = do
    print pe_002
    putStrLn ""
