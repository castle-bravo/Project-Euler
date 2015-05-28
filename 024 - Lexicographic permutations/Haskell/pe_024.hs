{-
file:   pe_024.hs
title:  Lexicographic permutations
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 25, 2015

link:   https://projecteuler.net/problem=24

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

-- Memoized factorial function    
factorial :: Int -> Int
factorial = (map factorial' [0..] !!)
  where
    factorial' 0 = 1
    factorial' n = n * factorial (n - 1)

-- Permute a finite list n times
permutation :: Eq a => Int -> [a] -> [a]
permutation 0 xs = xs -- 1st permutation in Project Euler convention
permutation n xs = x:permutation n' xs'
  where
    x   = xs !! (quot n f)
    n'  = mod n f
    xs' = filter (/= x) xs
    f   = factorial (length xs - 1)

-- The millionth permutation of a list is permutation number 999999
pe_024 = read (permutation 999999 ['0'..'9']) :: Int

main :: IO ()
main = do
    print pe_024

