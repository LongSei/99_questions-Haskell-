{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.List
import Data.Ord

-- Don't have a good solution for this problem :(

data Tree a = Empty | Node a (Tree a) | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

istree :: Tree a -> Bool 
istree (Node _ sub) = False 
istree Empty = True 
istree (Branch _ left right) = istree left && istree right
