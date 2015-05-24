{- |
file:   pe_017.hs
title:  Number letter counts
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 24, 2015

link:   https://projecteuler.net/problem=17

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

letterCount n
    | n < 20 = case n of
        0 -> 0
        1  -> 3 -- "one"
        2  -> 3 -- "two"
        3  -> 5 -- "three"
        4  -> 4 -- "four"
        5  -> 4 -- "five"
        6  -> 3 -- "six"
        7  -> 5 -- "seven"
        8  -> 5 -- "eight"
        9  -> 4 -- "nine"
        10 -> 3 -- "ten"
        11 -> 6 -- "eleven"
        12 -> 6 -- "twelve"
        13 -> 8 -- "thirteen"
        15 -> 7 -- "fifteen"
        18 -> 8 -- "eighteen"
        _  -> letterCount (n - 10) + 4 -- add "teen" suffix
    | n < 100 = case (quot n 10) of
        2 -> 6 + letterCount (rem n 10) -- "twenty"
        3 -> 6 + letterCount (rem n 10) -- "thirty"
        4 -> 5 + letterCount (rem n 10) -- "forty"
        5 -> 5 + letterCount (rem n 10) -- "fifty"
        8 -> 6 + letterCount (rem n 10) -- "eighty"
        _ -> 2 + letterCount (quot n 10) + letterCount (rem n 10)
             -- add "ty" suffix and letter count of LSD
    | n < 1000 = case (rem n 100) of
        0 -> 7 + letterCount (quot n 100) -- "hundred"
        _ -> 10 + letterCount (quot n 100) + letterCount (rem n 100)
             -- "hundred and"
    | otherwise = 11 -- "one thousand"

pe_017 = sum $ map letterCount [1..1000]

main :: IO ()
main = do
    print pe_017

