module RBTree.Test where


import Test.QuickCheck

import qualified RBTree as RB


instance (Ord a, Arbitrary a) => Arbitrary (RB.RBTree a) where
    arbitrary = do
        n <- choose (1,2) :: Gen Int
        case n of
            1 -> return RB.empty
            2 -> do
                x <- arbitrary
                y <- arbitrary
                return $ RB.insert x y


-- Insertion into a red-black tree results in a red-black tree
prop_rbtree_insert :: RB.RBTree Int  -> Int -> Bool
prop_rbtree_insert tree key = RB.isRedBlack tree &&
                              RB.isRedBlack (RB.insert tree key)



