import Data.Maybe
import Data.List
import Control.Monad
import Data.Bits
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

solving :: Tree a -> Int -> Int -> [a]
solving Empty request now = []
solving (Branch x left right) request now 
  | request == now = [x]
  | otherwise = solving left request (now + 1) ++ solving right request (now + 1)

atlevel :: Tree b -> Int -> [b]
atlevel xs request = solving xs request 1
