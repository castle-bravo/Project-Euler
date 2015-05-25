{-
file:   pe_045.hs
title:  Triangular, pentagonal, and hexagonal
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 25, 2015

link:   https://projecteuler.net/problem=45

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

triangular :: [Integer]
triangular = map (\x -> x*(x + 1) `quot` 2)   [1..]

pentagonal :: [Integer]
pentagonal = map (\x -> x*(3*x - 1) `quot` 2) [1..]

hexagonal  :: [Integer]
hexagonal  = map (\x -> x*(2*x - 1))          [1..]

intersection :: Ord a => [a] -> [a] -> [a] -> [a]
intersection []     (y:ys) (z:zs) = []
intersection (x:xs) []     (z:zs) = []
intersection (x:xs) (y:ys) []     = []
intersection (x:xs) (y:ys) (z:zs)
    | x < y     = intersection xs (y:ys) (z:zs)
    | y < z     = intersection (x:xs) ys (z:zs)
    | z < x     = intersection (x:xs) (y:ys) zs
    | otherwise = x:intersection xs ys zs -- x == y == z

pe_045 = head 
    $ filter ((<) 40755) 
    $ intersection triangular pentagonal hexagonal

main :: IO ()
main = do
    print pe_045
