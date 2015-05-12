{- |
file:   pe_008.hs
title:  Largest Product in a Series
author: Alexander P. Gosselin
e-mail: alexandergosselin@gmail.com
date:   May 12, 2015

link:   https://projecteuler.net/problem=8

copyright:  (C) 2015 Alexander Gosselin
license:    GNU General Public License <http://www.gnu.org/licenses/>
-}

import System.IO
import Data.Char

stripChars :: String -> String -> String
stripChars = filter . flip notElem

digitList :: String -> [Int]
digitList = (map digitToInt) . stripChars "\n"

windowProduct :: Int -> [Int] -> [Int] 
windowProduct n x
    | length x >= n = (product . take n) x : windowProduct n (tail x)
    | otherwise     = []

pe_008 :: String -> Int
pe_008 = maximum . windowProduct 13 . digitList

main :: IO ()
main = do
    contents <- readFile "../series.txt"
    print (pe_008 contents)
