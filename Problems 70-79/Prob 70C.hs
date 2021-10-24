import Data.Maybe
import Data.List
import Control.Monad
import Data.Bits
import Data.Tree

nnodes :: Tree a -> Int
nnodes = length . flatten
