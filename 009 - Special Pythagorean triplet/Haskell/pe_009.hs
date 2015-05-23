{-
  file:   pe_009.hs
  title:  Special Pythagorean triplet
  author: Alexander P. Gosselin
  e-mail: alexandergosselin@gmail.com
  date:   May 22, 2015
  
  link:   https://projecteuler.net/problem=9
-}

a :: Int -> [Int]
a n = [1..(quot n 3)]

b :: Int -> [(Int, Int)]
b n = filter (\(x, y) -> x < y)
      $ map (\x -> (x, quot (n^2 - 2*x*n) (2*(n - x)))) (a n)

c :: Int -> [(Int, Int, Int)]
c n = filter (\(x, y, z) -> y < z)
      $ map (\(x, y) -> (x, y, n - x - y)) (b n)

pythagoreanTriplet :: (Int, Int, Int) -> Bool
pythagoreanTriplet (x, y, z)
    | x^2 + y^2 == z^2  = True
    | otherwise         = False

product' :: (Int, Int, Int) -> Int
product' (x, y, z) = x*y*z

pe_009 = product' $ head $ filter pythagoreanTriplet (c 1000)

main :: IO ()
main = do
    print pe_009
