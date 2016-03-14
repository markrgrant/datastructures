-- A basic implementation of 23 trees, which ensure a balanced search tree
-- whose height is worst case ~lgN where N is the number of keys.  The
-- invariants maintained are:
-- 1. Symmetric order - inorder traversal yields keys in ascending order
-- 2. Perfect balance - every path from root to empty leaf has same length
module Tree23 (
    empty,
    search,
    insert
) where


data Tree23 a = Empty |
                Node2 a   (Tree23 a) (Tree23 a) |
                Node3 a a (Tree23 a) (Tree23 a) (Tree23 a) |
                Node4 a a a (Tree23 a) (Tree23 a) (Tree23 a) (Tree23 a) 
                deriving (Eq, Show)


empty :: (Ord a) => Tree23 a
empty = Empty


search :: (Ord a) => Tree23 a -> a -> Maybe a
search Empty _ = Nothing
search (Node2 k l r) x
    | x < k     = search l x
    | x == k    = Just x
    | otherwise = search r x
search (Node3 k1 k2 l m r) x
    | x < k1    = search l x
    | x == k1   = Just x
    | x < k2    = search m x
    | x == k2   = Just x
    | otherwise = search r x


-- Insert a key into the tree.  We need to handle the case of a 4-node
-- root that is turned into a 2-node with two 2-node children.
insert :: Ord a => Tree23 a -> a -> Tree23 a
insert tree x = 
    case root of
        Node4 k1 k2 k3 l m1 m2 r -> Node2 k2 (Node2 k1 l m1) (Node2 k3 m2 r)
        _                        -> root
    where root = insert' tree x


insert' Empty x = Node2 x Empty Empty
insert' n@(Node2 k Empty Empty) x
    | x < k     = Node3 x k Empty Empty Empty
    | x == k    = n -- no change
    | otherwise = Node3 k x Empty Empty Empty
insert' n@(Node3 k1 k2 Empty Empty Empty) x
    | x < k1    = Node4 x k1 k2 Empty Empty Empty Empty
    | x == k1   = n
    | x < k2    = Node4 k1 x k2 Empty Empty Empty Empty
    | x == k2   = n
    | otherwise = Node4 k1 k2 x Empty Empty Empty Empty
-- inserting into a non-leaf node.  Need to check for children that
-- may have become 4-nodes after doing the recursive insert.
insert' n@(Node2 k l r) x
    | x < k     = check4Node $ Node2 k (insert' l x) r
    | x == k    = n
    | otherwise = check4Node $ Node2 k l (insert' r x)
insert' n@(Node3 k1 k2 l m r) x
    | x < k1    = check4Node $ Node3 k1 k2 (insert' l x) m r
    | x == k1   = n
    | x < k2    = check4Node $ Node3 k1 k2 l (insert' m x) r
    | x == k2   = n
    | otherwise = check4Node $ Node3 k1 k2 l m (insert' r x)


-- If a child is a 4-node, address this by pushing its middle node up
-- into this node.  This may result in the parent node becoming a 4-node
-- itself.
check4Node :: Ord a => Tree23 a -> Tree23 a
-- 2-nodes become 3-nodes if they have a 4-node child.  Otherwise, no
-- change.
check4Node (Node2 k (Node4 y1 y2 y3 t1 t2 t3 t4) r) =
    Node3 y2 k (Node2 y1 t1 t2) (Node2 y3 t3 t4) r
check4Node (Node2 k l (Node4 y1 y2 y3 t1 t2 t3 t4)) =
    Node3 k y2 l (Node2 y1 t1 t2) (Node2 y3 t3 t4)
check4Node n = n
-- 3-nodes become 4-nodes if they have a 4-node child.  Otherwise, no
-- change.
check4Node (Node3 k1 k2 (Node4 y1 y2 y3 t1 t2 t3 t4) m r) = 
    Node4 y2 k1 k2 (Node2 y1 t1 t2) (Node2 y3 t3 t4) m r
check4Node (Node3 k1 k2 l (Node4 y1 y2 y3 t1 t2 t3 t4) r) = 
    Node4 k1 y2 k2 l (Node2 y1 t1 t2) (Node2 y3 t3 t4) r
check4Node (Node3 k1 k2 l m (Node4 y1 y2 y3 t1 t2 t3 t4)) = 
    Node4 k1 k2 y2 l m (Node2 y1 t1 t2) (Node2 y3 t3 t4)
check4Node n = n


instance Functor Tree23 where
    fmap f Empty = Empty
    fmap f (Node2 x l r) = Node2 (f x) (fmap f l) (fmap f r)
    fmap f (Node3 x1 x2 l m r) = Node3 (f x1) (f x2) (fmap f l) (fmap f m) (fmap f r)
