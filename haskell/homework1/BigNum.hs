{-
  Name: Kushagra Bainsla
  Class: CS 252
  Assigment: HW1
  Date: <Date assignment is due>
  Description: <Describe the program and what it does>
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0

bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] 0 = []
bigAdd' [] [] carry = [carry]
bigAdd' xs [] carry = bigAdd' xs [0] carry
bigAdd' [] ys carry = bigAdd' [0] ys carry
bigAdd' (x:xs) (y:ys) carry =
  newBlock : bigAdd' xs ys newCarry
  where
    s = x + y + carry
    newBlock = s `mod` maxblock
    newCarry = s `div` maxblock

bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [] [] 0 = []
bigSubtract' [] [] _ = error "Negative numbers not supported"
bigSubtract' (x:xs) [] borrow = bigSubtract' (x:xs) [0] borrow
bigSubtract' [] _ _ = error "Negative numbers not supported"
bigSubtract' (x:xs) (y:ys) borrow = 
  if diff >= 0 then diff : bigSubtract' xs ys 0
  else (diff + maxblock) : bigSubtract' xs ys 1
  where
    diff = x - y - borrow

bigEq :: BigNum -> BigNum -> Bool
bigEq [] [] = True
bigEq (x:xs) (y:ys) = x == y && bigEq xs ys
bigEq _ _ = False

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

-- Handle multiplication following the same approach you learned in grade
-- school, except dealing with blocks of 3 digits rather than single digits.
-- If you are having trouble finding a solution, write a helper method that
-- multiplies a BigNum by an Int.
bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply [] _ = [0]
bigMultiply _ [] = [0]
bigMultiply (x:xs) ys = reverse $ stripLeadingZeroes $ reverse $ bigAdd (mulByBlock ys x 0) (0 : bigMultiply xs ys)

mulByBlock :: BigNum -> Block -> Block -> BigNum
mulByBlock [] _ 0 = []
mulByBlock [] _ carry = [carry]
mulByBlock (b:bs) m carry = newBlock : mulByBlock bs m newCarry
  where
    prod = b * m + carry
    newBlock = prod `mod` maxblock
    newCarry = prod `div` maxblock

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf _ [0] = [1]
bigPowerOf x y = bigMultiply x (bigPowerOf x (bigDec y))

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]

sig = "9102llaf"
