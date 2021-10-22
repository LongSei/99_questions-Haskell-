import Data.Maybe
import Data.List
import Control.Monad
import Data.Bits
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves :: Tree a -> [a] 
countLeaves Empty = []
countLeaves (Branch x Empty Empty) = [x]
countLeaves (Branch _ left right) = countLeaves left ++ countLeaves right
