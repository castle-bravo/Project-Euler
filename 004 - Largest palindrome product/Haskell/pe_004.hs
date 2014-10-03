{- |
file:   pe_004.hs
title:  Largest palindrome product
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   September 29, 2014
-}

isPalindrome :: Integer -> Bool
isPalindrome x = 
    show x == (reverse . show) x

pe_004 = 
    maximum [ x*y | x <- [999,998..100], 
                    y <- [999,998..100], 
                    isPalindrome (x*y) ]

main :: IO ()
main = do
    print pe_004
