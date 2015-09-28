module Stack (
    empty, 
    isEmpty,
    cons,
    head,
    tail,
    (++),
    update,
    suffixes
) where

import Seq
import qualified Prelude as P (head, tail, (++))
import Prelude hiding (head, tail, (++))

data Stack a = Stack [a]

instance Seq Stack where
    empty = Stack []
    isEmpty (Stack xs) = null xs
    cons x (Stack xs) = Stack (x:xs)
    head (Stack xs) = P.head xs
    tail (Stack xs) = Stack (P.tail xs)
    (Stack []) ++ s = s
    (Stack (x:xs)) ++ s = Stack (x:ys) where (Stack ys) = Stack (xs P.++ ys)
    update (Stack []) _ _ = error "Empty"
    update (Stack (x:xs)) 0 y = Stack (y:xs)
    update (Stack (x:xs)) i y = Stack $ x:ys where (Stack ys) = update (Stack xs) (i-1) y
    suffixes (Stack xs)
        | null xs = Stack [[]]
        | otherwise = Stack (xs:ys) where (Stack ys) = suffixes (Seq.tail (Stack xs))


