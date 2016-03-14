-- A basic Tree data structure supporting arbitrary numbers of subtrees.

module Tree (
    create,
    addChild,
    size,
    height,
    toList
) where

data Tree a = EmptyTree | Node a [Tree a] deriving (Show)

create :: Tree a
create = EmptyTree

addChild :: a -> Tree a -> Tree a
addChild child EmptyTree = Node child []
addChild child (Node a children) = Node a ((Node child []):children)

size :: Tree a -> Int
size EmptyTree = 0
size (Node a children) = 1 + foldr (\a acc -> acc + size a) 0 children

height :: Tree a -> Int
height EmptyTree = 0
height (Node a children) = 1 + maximum (map height children)

toList :: Tree a -> [a]
toList EmptyTree = []
toList (Node x []) = [x]
toList (Node x xs) = x:(concat $ map toList xs)
