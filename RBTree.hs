-- A left-leaning red black balanced search tree implementation.
-- This is an alternative to the 2-3 tree data structure for providing
-- a balanced binary search tree.   The performance guarantees are the same,
-- but I was just curious to see how a Haskell implementation might differ.
--
-- The number of lines of code for insertion is about 1/2 that required for
-- the 2-3 tree implementation.
--
module RBTree (
    empty,
    search,
    insert
) where


data RBTree a = Empty | Node a (RBTree a) (RBTree a) Color
    deriving (Eq, Show)


data Color = Red | Black deriving (Eq, Show)


empty :: RBTree a
empty = Empty


search :: (Ord a) => RBTree a -> a -> Maybe a
search Empty _ = Nothing
search n@(Node k l r _) x
    | x < k = search l x
    | x > k = search r x
    | otherwise = Just x


insert :: (Ord a) => RBTree a -> a -> RBTree a
insert Empty x = Node x Empty Empty Red
insert n@(Node k l r c) x
    | x < k     = check $ Node k (insert l x) r c
    | x > k     = check $ Node k l (insert r x) c
    | otherwise = n
    where check = flipColors . rotateRight . rotateLeft


-- Rotate left if the right node is red and the left node is black (this
-- could include an Empty left node, which would be considered black).
-- Otherwise don't rotate left.
rotateLeft :: RBTree a -> RBTree a
rotateLeft (Node x Empty (Node y l2 r2 Red) c) =
    Node y (Node x Empty l2 Red) r2 c
rotateLeft (Node x l@(Node _ _ _ Black) (Node y l2 r2 Red) c) =
    Node y (Node x l l2 Red) r2 c
rotateLeft n = n


-- Rotate right if the left node is red and the left node of the left node
-- is red.  Otherwise don't rotate right.
rotateRight :: RBTree a -> RBTree a
rotateRight (Node x (Node y l2@(Node _ _ _ Red) r2 Red) r c) =
    Node y l2 (Node x r2 r Red) c
rotateRight n = n


-- If both children are red, flip their colors to black and flip this
-- node's color to red. 
flipColors :: RBTree a -> RBTree a
flipColors (Node k1 (Node k2 t1 t2 Red) (Node k3 t3 t4 Red) c) = 
    Node k1 (Node k2 t1 t2 Black) (Node k3 t3 t4 Black) Red
flipColors n = n
