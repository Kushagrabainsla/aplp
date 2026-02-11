import Data.List


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl func acc [] = acc
myFoldl func acc (x:xs) = myFoldl func (func acc x) xs


myReverse :: [a] -> [a]
myReverse (x:xs) = foldl (\acc y -> y : acc) [x] xs


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr func acc [] = acc
myFoldr func acc (x:xs) = func x (myFoldr func acc xs)


listAbs :: [Integer] -> [Integer]
listAbs = map abs


sumAbs :: [Integer] -> Integer
sumAbs = foldl (\acc xs -> acc + abs xs) 0



main :: IO ()
main = do
    print "Initializing classroom..."
    print $ myFoldl (+) 0 [1, 2, 3, 4, 5]
    print $ myFoldl (*) 1 [1, 2, 3, 4, 5]
    print $ myFoldr (:) [] [1, 2, 3, 4, 5]
    print $ myFoldr (++) [] ["Hello", " ", "world", "!"]
    print $ myReverse [1, 2, 3, 4, 5]
    print $ listAbs [-1, -2, -3, 4, 5]
    print $ sumAbs [-1, -2, -3, 4, 5]
