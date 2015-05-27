{-
file:   pe_065.hs
title:  Convergents of e
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 25, 2015

link:   https://projecteuler.net/problem=65

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

import Data.Char (digitToInt)

-- Continued fraction type alias
type ContinuedFraction = [Integer]

-- Continued fraction representation of e
e :: ContinuedFraction
e = 2:e' 1
  where
    e' k = 1:2*k:1:e' (k + 1)

-- List of numerators in numerator of convergent
numerators :: ContinuedFraction -> [Integer]
numerators cs = map numerator [0..]
  where
    numerator :: Int -> Integer
    numerator = (zipWith numerator' (1:cs) [0..] !!)
      where
        numerator' :: Integer -> Int -> Integer
        numerator' _ 0 = 1
        numerator' c 1 = c
        numerator' c n = c * numerator (n - 1) + numerator (n - 2)

-- List of digits in an integer
toDigits :: Integer -> [Int]
toDigits = (map digitToInt) . show

-- Sum of digit in numerator of 100th convergent of e
pe_065 = sum $ toDigits $ (numerators e) !! 100

main :: IO ()
main = do
    print $ pe_065
