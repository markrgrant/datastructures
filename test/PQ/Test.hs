module PQ.Test where

import Test.QuickCheck
import Data.Maybe (fromJust, Maybe(..))

import qualified PQ

instance (Ord a, Arbitrary a) => Arbitrary (PQ.PQ a) where
    arbitrary = do
        n <- choose (1,2) :: Gen Int
        case n of
            1 -> return PQ.empty
            2 -> do
                x <- arbitrary
                y <- arbitrary
                return $ PQ.insert x y


-- An empty priority queue has size zero
prop_pq_empty_size_0 :: Bool
prop_pq_empty_size_0 = PQ.size PQ.empty == 0


-- The maximum of a priority queue built from a list of values is the
-- maximum of that list of values
prop_pq_max :: [Int] -> Bool
prop_pq_max xs = if null xs
                    then isNothing result
                    else result == Just (maximum xs)
                    where result = PQ.max (PQ.fromList xs)


-- Deleting the maximum from a priority queue returns the largest value
-- in the queue and decreases its size by 1
prop_pq_delmax :: [Int] -> Bool
prop_pq_delmax xs 
    | null xs = isNothing $ PQ.delMax (PQ.fromList xs)
    | otherwise = max == maximum xs && PQ.size pq + 1 == length xs
                  where result = fromJust $ PQ.delMax (PQ.fromList xs)
                        max = snd result
                        pq = fst result


-- Insertion into a priority queue increases its size by 1
prop_pq_insert :: PQ.PQ Int -> Int -> Bool
prop_pq_insert q n = PQ.size (PQ.insert q n) == PQ.size q + 1
