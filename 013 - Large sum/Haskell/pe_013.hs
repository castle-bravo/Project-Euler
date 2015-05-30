{-
file:   pe_013.hs
title:  Large sum
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 30, 2015

link:   https://projecteuler.net/problem=13

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>

note 1:  Since there are 100 numbers to add, digits in the twelfth
  position will affect the value of the 10th digit of the sum, but
  digits after the 12th digit do not affect any of the first 10
  digits of the sum, so we can ignore them.
          
note 2:  When summing over 100 12-digit numbers, we get a 14-digit
  number. We can divide this by 10000 to shave off the last 4 digits.
-}

readInt :: String -> Int
readInt x = read x :: Int

pe_013 = (flip quot 10000)
         . sum 
         . map readInt
         . map (take 12) 
         . words

-- alternate implementation that takes the same amount of time
{-
pe_013 = readInt
         . take 10
         . show
         . sum 
         . map readInt
         . map (take 12) 
         . words
-}

main :: IO ()
main = do
    contents <- readFile "../numbers.txt"
    print (pe_013 contents)
