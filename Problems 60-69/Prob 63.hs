import Data.Maybe
import Data.List
import Control.Monad
import Data.Bits
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

fulltree :: a -> Int -> Tree a 
fulltree x 0 = Empty
fulltree x 1 = leaf x
fulltree x remain = Branch x (fulltree x (remain - 1)) (fulltree x (remain - 1))

mrmax :: Int -> Int -> Int -> Int
mrmax request curr level 
  | curr > request = level - 1
  | curr == request = level 
  | otherwise = mrmax request (curr + (2 ^ level)) (level + 1)

maxmore :: Int -> Int 
maxmore n = mrmax n 0 0

total :: Int -> Int 
total 0 = 0
total 1 = 1
total level = 2 ^ (level - 1) + total (level - 1)

completeBinaryTree :: Int -> Tree Char 
completeBinaryTree 0 = Empty
completeBinaryTree 1 = leaf 'x'
completeBinaryTree 2 = Branch 'x' (leaf 'x') Empty
completeBinaryTree 3 = Branch 'x' (leaf 'x') (leaf 'x')
completeBinaryTree node 
  | remain == 0 = Branch 'x' (fulltree 'x' (morelevel - 1)) (fulltree 'x' (morelevel - 1))
  | remain >= 2 ^ (morelevel - 1) = Branch 'x' (fulltree 'x' morelevel) (completeBinaryTree $ remain - 1)
  | otherwise = Branch 'x' (completeBinaryTree $ 2 ^ (morelevel - 1) - 1 + remain) (fulltree 'x' $ morelevel - 1)
  where morelevel = maxmore node 
        remain = node - (2 ^ morelevel) + 1

countNode :: Tree Char -> Int 
countNode Empty = 0
countNode (Branch 'x' left right) = 1 + countNode left + countNode right 

compareTree :: Tree a -> Tree a -> Bool 
compareTree Empty (Branch _ left right) = False 
compareTree (Branch _ left right) Empty = False 
compareTree Empty Empty = True 
compareTree (Branch _ left1 right1) (Branch _ left2 right2) 
  | compareTree left1 left2 && compareTree left2 right2 = True 
  | otherwise = False

isCompleteBinaryTree :: Tree Char -> Bool 
isCompleteBinaryTree x = compareTree x (completeBinaryTree $ countNode x)
