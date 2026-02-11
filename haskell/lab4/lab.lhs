> import Data.List

Experiment with foldl, foldr, and foldl'

First, implement your own version of the foldl function,
defined as myFoldl

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl func acc [] = acc
> myFoldl func acc (x:xs) = myFoldl func (func acc x) xs


Next, define a function to reverse a list using foldl.

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:xs) = foldl (\acc y -> y : acc) [x] xs


Now define your own version of foldr, named myFoldr

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr func acc [] = acc
> myFoldr func acc (x:xs) = func x (myFoldr func acc xs)


Now try using foldl (the library version, not yours) to sum up the numbers of a large list.
Why is it so slow?
It is slow because foldl is a lazy function that builds up thunks (deferred computations) for each step of the fold.
When processing a large list, it creates a large thunk that needs to be evaluated at the end.
This can lead to excessive memory usage and slow performance due to the overhead of managing thunks.


Instead of foldl, try using foldl'.
Why is it faster? (Read http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 for some hints)
It is faster because foldl' is a strict version of foldl, which means it evaluates the accumulator at each step instead of building up thunks.
This allows it to process large lists without consuming excessive memory, as it does not create thunks that need to be evaluated later.
By evaluating the accumulator immediately, foldl' can efficiently handle large lists without running into stack overflow issues.


For an extra challenge, try to implement foldl in terms of foldr.
See http://www.haskell.org/haskellwiki/Foldl_as_foldr for details.


Next, using the map function, convert every item in a list to its absolute value

> listAbs :: [Integer] -> [Integer]
> listAbs = map abs

Finally, write a function that takes a list of Integers and returns the sum of
their absolute values.

> sumAbs :: [Integer] -> Integer
> sumAbs = foldl (\acc x -> acc + abs x) 0