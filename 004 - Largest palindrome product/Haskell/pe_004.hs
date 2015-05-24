{- |
file:   pe_004.hs
title:  Largest palindrome product
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 24, 2015

link:   https://projecteuler.net/problem=4

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

concatenate :: [[a]] -> [a]
concatenate = foldr (++) []

-- products is pre-sorted so that only the largest palindrome product
-- is calculated.
products :: Integer -> [Integer]
products m = concatenate $ map diagonalProducts [m, m - 1..1]
  where
    diagonalProducts n = diagonal ++ diagonal'
      where
        diagonal  = [ n^2 - k^2 | k <- [0..n-1], n + k <= m]
        diagonal' = [ n^2 - k^2 - n - k | k <- [0..n - 1], 
                                          n^2 - k^2 - n - k > 0,
                                          n + k <= m]

isPalindrome :: Integer -> Bool
isPalindrome x = 
    show x == (reverse . show) x

largestPalindromeProduct :: Integer -> Integer
largestPalindromeProduct m = head palindromeProducts
  where
    palindromeProducts = filter isPalindrome $ products m

pe_004 = largestPalindromeProduct 999

main :: IO ()
main = do
    print $ pe_004

