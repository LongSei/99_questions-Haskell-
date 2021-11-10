{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.List
import Data.Ord

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

tree = Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
dotstring = "xy..a.b.."
                                                       
solve :: String -> (Tree Char, String) 
solve [] = (Empty, "")
solve ('.':xs) = (Empty, xs)          
solve (x:xs) = (Branch x left right, remain)
        where (left, rest) = solve xs 
              (right, remain) = solve rest                

dstotree :: String -> Tree Char 
dstotree xs = fst $ solve xs                                                                               
                                                                                                                                                               
treetods :: Tree Char -> String
treetods Empty = "."                                                                                                                                            
treetods (Branch x l r) = x:(treetods l ++ treetods r)
