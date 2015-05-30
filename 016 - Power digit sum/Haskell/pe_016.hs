{-
file:   pe_016.hs
title:  Power digit sum
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 30, 2015

link:   https://projecteuler.net/problem=16

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

import Data.Char (digitToInt)

pe_016 = sum $ map digitToInt $ show (2^1000)

main :: IO ()
main = do
    print pe_016

