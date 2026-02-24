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
bigAdd x y = bigAddHelper x y 0

bigAddHelper :: BigNum -> BigNum -> Block -> BigNum
bigAddHelper [] [] 0 = []
bigAddHelper [] [] carry = [carry]
bigAddHelper xs [] carry = bigAddHelper xs [0] carry
bigAddHelper [] ys carry = bigAddHelper [0] ys carry
bigAddHelper (x:xs) (y:ys) carry =
  newBlock : bigAddHelper xs ys newCarry
  where
    s = x + y + carry
    newBlock = s `mod` maxblock
    newCarry = s `div` maxblock

bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtractHelper x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtractHelper :: BigNum -> BigNum -> Block -> BigNum
bigSubtractHelper [] [] 0 = []
bigSubtractHelper [] [] _ = error "Negative numbers not supported"
bigSubtractHelper (x:xs) [] borrow = bigSubtractHelper (x:xs) [0] borrow
bigSubtractHelper [] _ _ = error "Negative numbers not supported"
bigSubtractHelper (x:xs) (y:ys) borrow = 
  if diff >= 0 then diff : bigSubtractHelper xs ys 0
  else (diff + maxblock) : bigSubtractHelper xs ys 1
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
-- (The bigMultiply function works just like grade school multiplication. It takes the first block from the first number and multiplies it against the entire second number using multiplyByBlock. Then it recursively multiplies the remaining blocks of the first number with the second number. The crucial part is the 0 : prefix before the recursive call - this shifts the recursive result left by one block position (equivalent to multiplying by 1000), just like how you write each partial product one position to the left in grade school multiplication. Finally, it adds these two partial products together using bigAdd. The bigAdd function handles lists of different lengths automatically, so when you add [984, 360] and [0, 147, 97], it aligns them by position: position 0 adds 984+0, position 1 adds 360+147, and position 2 adds nothing+97, giving [984, 507, 97]. The process repeats recursively until all blocks of the first number have been processed, building up the final result through successive additions of shifted partial products.)
bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply [] _ = [0]
bigMultiply _ [] = [0]
bigMultiply (x:xs) ys = reverse $ stripLeadingZeroes $ reverse $ bigAdd (multiplyByBlock ys x 0) (0 : bigMultiply xs ys)

multiplyByBlock :: BigNum -> Block -> Block -> BigNum
multiplyByBlock [] _ 0 = []
multiplyByBlock [] _ carry = [carry]
multiplyByBlock (b:bs) m carry = newBlock : multiplyByBlock bs m newCarry
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
