{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

data Tree a = Node a [Tree a] deriving (Eq, Show)

bottom_up :: Tree Char -> String 
bottom_up (Node x ts) = concatMap bottom_up ts ++ [x] 
