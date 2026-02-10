-- USING MAP, convert a list of Strings to a list of Integers.
stringsToNums :: [String] -> [Integer]
stringsToNums = map read


-- USING MAP, take a list of integers and return a list with
-- the equivalent absolute values.
mapAbsVal :: [Integer] -> [Integer]
mapAbsVal = map abs


-- USING ZIPWITH, concatenate a list of first names with
-- a list of last names to produce a list of full names.
join x y = x ++ " " ++ y
fullNames :: [String] -> [String] -> [String]
fullNames = zipWith join

-- USING MAP AND FILTER, square all positive numbers in a list;
-- strip out 0 and negative numbers.
square x = x*x
positive x = if x > 0 then True else False
squarePostives :: [Integer] -> [Integer]
squarePostives = map square . filter positive

main :: IO ()
main = do
  print $ stringsToNums []
  print $ stringsToNums ["1", "2", "3", "4", "5"]
  print $ mapAbsVal [-4, 3, 2, -99, 54]
  print $ mapAbsVal $ stringsToNums ["-4", "7", "-22"]
  print $ fullNames ["John", "Wes Happen", "Holly"] ["Smith", "Ng", "Wood"]
  print $ squarePostives [2, -3, 45, 5, 0, 7, -6]
