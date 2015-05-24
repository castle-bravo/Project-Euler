{-
file:   pe_020.hs
title:  Number letter counts
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 24, 2015

link:   https://projecteuler.net/problem=20

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

import Data.Char (ord)

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

pe_020 = sum $ map (flip (-) 48 . ord) $ show $ factorial 100

main :: IO ()
main = do
    print pe_020
