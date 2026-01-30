module Main where

main :: IO ()
main = do
  print (maxNum [1,2,3])
  print (maxNum [4])
  print $ maxNum [7, 12, 0, 99, 46, 12349, 82] -- note the use of the '$' here
  print $ maxNum [-4, -1, -99, -2]
  print $ maxNum [7, -9, 12, 0, 99, -3, 46, 12349, 82, -7, 8]
  print $ maxNum []


maxNum :: [Integer] -> Integer
maxNum [] = error "empty list"
maxNum [x] = x
maxNum (x:xs) = 
   let maxx = maxNum xs
   in 
   if maxx > x then maxx else x
