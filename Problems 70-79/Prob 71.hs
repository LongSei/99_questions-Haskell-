{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

data Tree a = Node a [Tree a] deriving (Eq, Show)

ipl :: Tree a -> Int 
ipl = counting 0 
    where counting n (Node x xs) = n + sum (map (counting (n + 1)) xs)
