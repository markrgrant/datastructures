-- A heap-based priority queue implementation that provides O(lgN) guaranteed
-- performance for insert and delete, and O(1) performance for max.
--
module PQ (
    PQ,
    empty,    -- O(1)
    fromList, -- O(N)
    insert,   -- O(lgN)
    max,      -- O(1)
    delMax,   -- O(lgN)
    isEmpty,  -- O(1)
    size      -- O(1)
) where


import Prelude hiding (max)
import Data.List (foldl')
import qualified Data.Vector as V


data PQ a = PQ (V.Vector a) Int deriving (Eq, Show)


-- Create an empty priority queue
empty :: PQ a
empty = PQ V.empty 0


-- Create a priority queue from a list of keys
fromList :: (Ord a) => [a] -> PQ a
fromList xs = foldl' (\pq x -> insert pq x) empty xs


-- Insert a key into the priority queue, returning a priority queue that
-- contains the key.
insert :: (Ord a) => PQ a -> a -> PQ a
insert pq@(PQ v s) x
    | isEmpty pq = PQ (V.fromList [x,x]) 1
    | V.length v > s = let v' = v V.// [(s, x)]
                       in PQ (swim v' s) (s+1)
    | otherwise = let v' = v V.++ v
                  in PQ (swim (v' V.// [(s, x)]) s) (s+1)


-- Retrieve the key with the highest priority, leaving it in the priority
-- queue.
max :: (Ord a) => PQ a -> Maybe a
max (PQ vec size)
    | size == 0 = Nothing
    | otherwise = Just $ vec V.! 0


-- Retrieve the key with the highest priority, removing it from the priority
-- queue.
delMax :: (Ord a) => PQ a -> Maybe (PQ a, a)
delMax (PQ v s)
    | s == 0 = Nothing
    | otherwise = let max = v V.! 0
                      v' = swap v 0 (s-1)
                  in Just (PQ (sink v' 0 (s-1)) (s-1), max)
               

-- Returns True if the priority queue has no keys, False otherwise.
isEmpty :: PQ a -> Bool
isEmpty (PQ _ s) = s == 0


-- Return the number of keys in the priority queue.
size :: PQ a -> Int
size (PQ _ s) = s


-- Given an index in the heap, if the parent value is less than the value
-- at the index, swap the child value with the parent value and continue
-- until it is smaller than its parent.
swim :: (Ord a) => V.Vector a -> Int -> V.Vector a
swim v i
    | i <= 0 = v                                           -- done
    | v V.! (parent i) >= v V.! i = v                    -- done 
    | otherwise = swim (swap v i (parent i)) (parent i)


sink :: (Ord a) => V.Vector a -> Int -> Int -> V.Vector a
sink v i s
    | right i > s-1 = v  -- cannot sink any further
    | v V.! i >= v V.! ci = v  -- done sinking
    | otherwise = sink (swap v i ci) ci s
    where ci = maxchild v i


swap :: V.Vector a -> Int -> Int-> V.Vector a
swap v i j = let vi = v V.! i
                 vj = v V.! j
             in v V.// [(i,vj),(j, vi)]


parent :: Int -> Int
parent i = (i-1) `div` 2


maxchild :: (Ord a) => V.Vector a -> Int -> Int
maxchild v i = if (v V.! (left i)) > (v V.! (right i))
                   then (left i)
                   else (right i)


left :: Int -> Int
left i = 2*i+1


right :: Int -> Int
right i = 2*i+2

