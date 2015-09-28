-- a binary search tree
data BSTree a =
    EmptyTree | Node a (BSTree a) (BSTree a)
    deriving (Show, Read, Eq)

isEmpty :: (Ord a) => BSTree a -> Bool
isEmpty EmptyTree = True
isEmpty tree = False

insert :: (Ord a) => a -> BSTree a -> BSTree a
insert a EmptyTree =  Node a EmptyTree EmptyTree
insert a (Node x left right)
    | a == x = Node a left right
    | a < x = Node x (insert a left) right
    | a > x = Node x left (insert a right)

isElem :: (Ord a) => a -> BSTree a -> Bool
isElem x EmptyTree = False
isElem x (Node a left right)
    | a == x = True
    | a < x = isElem x left
    | a > x = isElem x right
