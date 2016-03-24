module Set (
    empty,
    insert,
    member
) where


import BSTree as BST


newtype Set a = Set (BSTree a)

instance Functor Set where
    fmap f (Set (BSTree left x right)) =
        Set (BSTree (f x) (fmap f left) (fmap f right))
    

empty :: (Ord a) => Set a
empty = Set BST.empty

insert :: (Ord a) => Set a -> a -> Set a
insert (Set tree) x = BST.insert x tree

member :: (Ord a) => Set a -> a -> Bool
member (Set tree) s = BST.isElem x tree
