{-
  file:   pe_001.hs
  title:  Multiples of 3 and 5
  author: Alexander P. Gosselin
  e-mail: alexandergosselin@gmail.com
  date:   September 20, 2014
-}

pe_001 = sum [ x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

main :: IO ()
main = do
    print pe_001
    putStrLn ""
