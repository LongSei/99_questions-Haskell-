import Data.Maybe
import Data.List
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

cbaltree :: Int -> [Tree Char]
cbaltree 0 = [Empty]
cbaltree n = let (s, r) = (n - 1) `quotRem` 2 in [Branch 'x' left right | i <- [s .. s + r], left <- cbaltree i, right <- cbaltree (n - 1 - i)]
