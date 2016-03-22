{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import System.Exit (exitSuccess, exitFailure)
import Data.Maybe (fromJust, Maybe(..))

import qualified RBTree as RB
import qualified PQ as PQ
import qualified Graph as G
import qualified DepthFirstPaths as DP

import Data.List (foldl')


instance (Ord a, Arbitrary a) => Arbitrary (RB.RBTree a) where
    arbitrary = do
        n <- choose (1,2) :: Gen Int
        case n of
            1 -> return RB.empty
            2 -> do
                x <- arbitrary
                y <- arbitrary
                return $ RB.insert x y


instance (Ord a, Arbitrary a) => Arbitrary (PQ.PQ a) where
    arbitrary = do
        n <- choose (1,2) :: Gen Int
        case n of
            1 -> return PQ.empty
            2 -> do
                x <- arbitrary
                y <- arbitrary
                return $ PQ.insert x y


instance Arbitrary G.Graph where
    arbitrary = do
        n <- choose (1,2) :: Gen Int
        case n of
            1 -> do -- create a graph
                numEdges <- choose (0, 100)
                return $ G.create numEdges
            2 -> do -- add an edge to a graph
                g <- arbitrary
                fr <- choose (0, (G.numVertices g) - 1)
                to <- choose (0, (G.numVertices g) - 1)
                return $ G.addEdge g fr to


-- Create an arbitrary Paths instance from an arbitrary Graph
-- and an arbitrary vertex within that graph as the starting point.
instance Arbitrary DP.Path where
    arbitrary = do
        graph <- arbitrary
        vertex <- choose (0, (G.numVertices g) - 1)
        return $ DP.create graph vertex


-- Insertion into a red-black tree results in a red-black tree
prop_rbtree_insert :: RB.RBTree Int  -> Int -> Bool
prop_rbtree_insert tree key = RB.isRedBlack tree &&
                              RB.isRedBlack (RB.insert tree key)


-- An empty priority queue has size zero
prop_pq_empty_size_0 :: Bool
prop_pq_empty_size_0 = PQ.size PQ.empty == 0


-- The maximum of a priority queue built from a list of values is the
-- maximum of that list of values
prop_pq_max :: [Int] -> Bool
prop_pq_max xs = if length xs == 0
                    then result == Nothing
                    else result == Just (maximum xs)
                    where result = PQ.max (PQ.fromList xs)


-- Deleting the maximum from a priority queue returns the largest value
-- in the queue and decreases its size by 1
prop_pq_delmax :: [Int] -> Bool
prop_pq_delmax xs 
    | length xs == 0 = PQ.delMax (PQ.fromList xs) == Nothing
    | otherwise = max == maximum xs && PQ.size pq + 1 == length xs
                  where result = fromJust $ PQ.delMax (PQ.fromList xs)
                        max = snd result
                        pq = fst result


-- Insertion into a priority queue increases its size by 1
prop_pq_insert :: PQ.PQ Int -> Int -> Bool
prop_pq_insert q n = PQ.size (PQ.insert q n) == (PQ.size q) + 1




-- necessary for quickCheckAll to function properly
return []


main :: IO ()
main = do
    result <- $quickCheckAll
    if result
        then exitSuccess
        else exitFailure
