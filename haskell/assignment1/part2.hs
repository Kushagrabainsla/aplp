module Main where

main :: IO ()
main = do
  print (fizzbuzz 1)
  print (fizzbuzz 7)
  print $ fizzbuzz 99
  print $ fizzbuzz 0
  print $ fizzbuzz (-2)


fizzbuzz :: Int -> String
fizzbuzz 0 = ""
fizzbuzz 1 = "1"
fizzbuzz n = 
 if n > 0 then fizzbuzz (n-1) ++ " " ++ fizz n
 else                       error "Non-negative numbers only"

fizz :: Int -> String
fizz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3  == 0 = "Fizz"
  | n `mod` 5  == 0 = "Buzz"
  | otherwise       = show n