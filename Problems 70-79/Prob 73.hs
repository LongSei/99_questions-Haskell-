{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.List
import Data.Ord

data Tree a = Node a [Tree a] deriving(Show, Eq)
tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

lisp :: Tree Char -> String
lisp (Node x xs) = x : foldl (\acc al@(Node a as) -> acc ++ "(" ++ lisp al ++ ")") [] xs

