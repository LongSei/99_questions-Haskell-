import Data.Maybe
import Data.List
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

checking :: Tree a -> Tree a -> Bool 
checking Empty Empty = True
checking Empty Branch {} = False
checking Branch {} Empty = False
checking (Branch _ left right) (Branch _ left' right') = checking left left' && checking right right'

istree :: Tree a -> Bool 
istree Empty = True
istree (Branch _ left right) = checking left right
