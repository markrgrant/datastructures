-- A heap-based priority queue implementation that provides O(lgN) guaranteed
-- performance for insert and delete, and O(1) performance for max.
module PQ (
    empty,
    fromList,
    insert,
    max,
    delMax,
    isEmpty,
    size
) where


import qualified Data.Vector as V


newtype PQ a = PQ (Data.Vector a)


empty :: PQ a
empty = PQ V.empty


fromList :: (Ord a) => [a] -> PQ a
fromList lst = foldl' (\lst item -> insert lst item) empty lst


insert :: (Ord a) => PQ a -> a -> PQ a
insert = undefined


max :: (Ord a) => PQ a -> a
max = undefined


delMax :: PQ a -> (PQ a, a)
delmax = undefined


isEmpty :: PQ a -> Bool
isEmpty = undefined


size :: PQ a -> Int
size (PQ vec) = V.length vec
