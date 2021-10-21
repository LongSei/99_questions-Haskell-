import Data.Maybe
import Data.List
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

hbalTree :: a -> Int -> [Tree a]
hbalTree x 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x n = [Branch x left right | (l, r) <- [(n - 1,n - 1), (n - 1,n - 2), (n - 2,n - 1)], left <- hbalTree x l, right <- hbalTree x r]
