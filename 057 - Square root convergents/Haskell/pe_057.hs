{-
file:   pe_057.hs
title:  Square root convergents
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   October 6, 2014

link:   https://projecteuler.net/problem=57

note:   logBase 10 x returns Infinity when the size of x exceeds the
        maximum size of a floating point number. If both the numerator
        and denominator are larger than this value, then the number of
        digits they have will falsely be reported as the same.
        
note 2: I submitted this solution to codereview.stackexchange.com.
        http://codereview.stackexchange.com/q/64916/54309
-}

numerator = 
  3 : 7 : zipWith (+) numerator (map ((*) 2) (tail numerator))

denominator = 
  2 : 5 : zipWith (+) denominator (map ((*) 2) (tail denominator))

numDigits :: Integer -> Int
numDigits = length . show

moreDigits :: Integer -> Integer -> Bool
moreDigits x y = (numDigits x) > (numDigits y)

countTrue :: [Bool] -> Int
countTrue = length . filter ((&&) True)

pe_057 = countTrue $ 
  take 1000 (zipWith moreDigits numerator denominator)

main :: IO ()
main = do
    print pe_057
