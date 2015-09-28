-- a Bag is a container of objects to be iterated 
-- over. The Bag provides a function for testing
-- emptiness, and a count of items in the bag.
--
-- Although a Bag need not contain items of the same type,
-- I'm not exactly sure how to support this in Haskell.
module Bag where


data Bag a = Bag [a] Int

add :: Bag -> a -> Bag
add (Bag xs sz) item = Bag (x:xs) (sz + 1)
    

isEmpty :: Bag -> Bool
isEmpty bag = size bag == 0


size :: Bag -> Int
size (Bag _ sz) = sz



