> data Tree v =
>     Empty
>   | Node v (Tree v) (Tree v)
>   deriving (Show)

> instance Functor Tree where
>    fmap f Empty = Empty
>    fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)


> main = print $ fmap (+1) (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))