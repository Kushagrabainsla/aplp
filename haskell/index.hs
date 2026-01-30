module Main where

main :: IO ()
main = do
  putStrLn "Hello!"
  print (double 21)
  print (sumEvens [1..10])

double :: Int -> Int
double x = 2 * x

sumEvens :: [Int] -> Int
sumEvens = sum . filter even
