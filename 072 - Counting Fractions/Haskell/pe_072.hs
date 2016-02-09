{-
file:   pe_072.hs
title:  Counting fractions
author: Alexander Gosselin
e-mail: alexandergosselin@gmail.com
        alexander.gosselin@alumni.ubc.ca
date:   February 7, 2016

link:   <https://projecteuler.net/problem=5>

note:   Regretfully, this is a copy of Zach Denton's solution.
        <https://zach.se/project-euler-solutions/72/>
        I had already solved the problem in C++, but I was having a
        hard time writing a memory-efficient solution in Haskell. My
        attempt at a solution is commented out below. It can only be
        used to compute totientSum n in reasonable time for n less than 
        a few tens of thousands.
        
references:
    Farey sequence:
        <http://mathworld.wolfram.com/FareySequence.html>
        <https://en.wikipedia.org/wiki/Farey_sequence>
-}

farey :: Int -> Int
farey = (map farey' [0..] !!)
  where
    farey' n = (quot (n*(n + 3)) 2) 
               - sum [farey $ quot n x | x <- [2..n]]

totientSum n = farey n - farey 1

pe_072 = totientSum 1000000 

main :: IO ()
main = do
  print pe_072

{-
-- <https://www.reddit.com/2tpqip/>
--applyEvery :: (a -> a) -> Int -> [a] -> [a]
--applyEvery f n = zipWith ($) (cycle (replicate (n - 1) id ++ [f]))

-- more memory efficient than above implementation
applyEvery :: (a -> a) -> Int -> [a] -> [a]
applyEvery f n xs = xf ++ (\(y:ys) -> f y : applyEvery f n ys) xb
  where
    (xf, xb) = splitAt (n - 1) xs

totients :: [Int]
totients = 1 : sieve [2..] [2..]
  where 
    sieve (x:xs) (y:ys) 
        | x == y    = (y - 1) : sieve xs (propagatePrime x ys)  
        | otherwise = y : sieve xs ys
    propagatePrime j = applyEvery (\x -> (quot x j)*(j - 1))

totientSum :: Int -> Int
totientSum n = sum $ take n totients
-}
