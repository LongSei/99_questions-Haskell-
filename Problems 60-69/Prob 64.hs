{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.Maybe
import Data.List
import Control.Monad
import Data.Bits
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

countNode :: Tree a -> Int 
countNode Empty = 0
countNode (Branch _ left right) = 1 + countNode left + countNode right

solving :: Tree a -> Int -> Int -> Tree (a, (Int, Int))
solving (Branch x Empty Empty) accleft level = Branch (x, (accleft + 1, level)) Empty Empty
solving (Branch x left Empty) accleft level = Branch (x, (accleft + countNode left + 1, level)) (solving left accleft $ level + 1) Empty 
solving (Branch x Empty right) accleft level = Branch (x, (accleft + 1, level)) Empty (solving right (accleft + 1) $ level + 1)
solving (Branch x left right) accleft level = Branch (x, (accleft + countNode left + 1, level)) (solving left accleft $ level + 1) (solving right (accleft + countNode left + 1) $ level + 1)

layout :: Tree a -> Tree (a, (Int, Int))
layout x = solving x 0 1
