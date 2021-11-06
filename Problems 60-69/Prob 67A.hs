{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
type Pos = (Int, Int)
leaf x = Branch x Empty Empty

string1 = "x(y,a(,b))"
tree1 = Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) =
        x : '(' : treeToString l ++ "," ++ treeToString r ++ ")"

optimize a@(x:xs) | x == ',' || x == ')' = (a, Empty) -- 
optimize (x:y:xs) | y == ',' || y == ')' = (y:xs, Branch x Empty Empty)
                  | y == '(' = (xs'', Branch x l r)
                  where (',':xs', l) = optimize xs 
                        (')':xs'', r) = optimize xs'

stringTotree :: String -> Tree Char
stringTotree "" = Empty
stringTotree [x] = Branch x Empty Empty
stringTotree string = snd (optimize string)
