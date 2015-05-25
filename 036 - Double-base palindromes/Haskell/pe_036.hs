{-
file:   pe_036.hs
title:  Double-base palindromes
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 25, 2015

link:   https://projecteuler.net/problem=36

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

import Data.Bits

readInteger :: [Char] -> Integer
readInteger x = read x :: Integer

mirrors :: Integer -> [Integer]
mirrors n = mirrorOdd n : mirrorEven n : []
  where
    mirrorOdd n = readInteger $ (show n) ++ (tail . reverse . show) n
    mirrorEven n = readInteger $ (show n) ++ (reverse . show) n

decimalPalindromes :: [Integer]
decimalPalindromes = foldr (++) [] $ map mirrors [1..]

binaryReverse :: Integer -> Integer
binaryReverse n = binaryReverse' 0 n
  where
    binaryReverse' r n
        | n == 0    = r
        | otherwise = 
            binaryReverse' (shift r 1 .|. (n .&. 1)) (shift n (-1))

isBinaryPalindrome :: Integer -> Bool
isBinaryPalindrome n = n == binaryReverse n

doubleBasePalindromes :: [Integer]
doubleBasePalindromes = filter isBinaryPalindrome decimalPalindromes

pe_036 = sum $ takeWhile ((>) 1000000) doubleBasePalindromes

main :: IO ()
main = do
    print pe_036
