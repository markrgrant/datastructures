module Seq where


-- The Seq class represents containers which maintain
-- elements in order like queues, deques, lists
class Seq s where
    empty :: s a
    isEmpty :: s a -> Bool
    cons :: a -> s a -> s a
    head :: s a -> a       -- raises Empty if stack is empty, use with caution
    tail :: s a -> s a     -- raises Empty if stack is empty, use with caution
    (++) :: s a -> s a -> s a
    update :: s a -> Int -> a -> s a -- raises Empty if stack is empty, use with caution
    suffixes :: s a -> s [a]
