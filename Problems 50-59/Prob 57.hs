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

symmetric :: Tree a -> Bool 
symmetric Empty = True
symmetric (Branch _ left right) = checking left right

insertTree :: Ord a => a -> Tree a -> Tree a  
insertTree n Empty = leaf n
insertTree n (Branch m left right) = if n < m then Branch m (insertTree n left) right else Branch m left (insertTree n right)

construct :: Ord a => [a] -> Tree a
construct xs = foldl (flip insertTree) Empty xs
