{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.List
import Data.Ord

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

tree = Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

treetopre :: Tree Char -> String
treetopre Empty = ""
treetopre (Branch x left right) = [x] ++ treetopre left ++ treetopre right

treetoin :: Tree Char -> String
treetoin Empty = ""
treetoin (Branch x left right) = treetoin left ++ [x] ++ treetoin right

splitbyfunc :: (a -> Bool) -> [a] -> ([a], [a])
splitbyfunc f xs = (left, right)
   where left = takeWhile f xs
         right = reverse . takeWhile f $ reverse xs

takewithorder :: String -> String -> String
takewithorder xs ys = foldl (\acc x -> if x `elem` ys then acc ++ [x] else acc) [] xs

preintree :: String -> String -> Tree Char
preintree "" "" = Empty
preintree preor inor = Branch (head preor) (preintree left' left) (preintree right' right)
        where (left, right) = splitbyfunc (/= head preor) inor
              left' = takewithorder preor left
              right' = takewithorder preor right
