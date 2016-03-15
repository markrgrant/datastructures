import Test.QuickCheck




prop_revapp :: [Int] -> [Int] -> Bool
prop_revap xs ys = reverse (xs++ys) == reverse xs ++ reverse ys




main = quickcheck prop_revapp
