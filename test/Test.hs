import Test.QuickCheck

import RBTree


instance (Ord a, Arbitrary a) => Arbitrary (RBTree a) where
    arbitrary = do
        n <- choose (1,2) :: Gen Int
        case n of
            1 -> return empty
            2 -> do
                x <- arbitrary
                y <- arbitrary
                return $ insert x y


propInsert :: RBTree Int  -> Int -> Bool
propInsert tree key = isRedBlack tree && isRedBlack (insert tree key)


main = quickCheck propInsert
