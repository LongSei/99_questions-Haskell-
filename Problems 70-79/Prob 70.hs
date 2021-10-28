{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

data Tree a = Node a [Tree a] deriving (Eq, Show)

solving (x:xs)
        | x == '^' = ([], xs)
        | otherwise = (Node x ftree : stree, srest)
        where (ftree, frest) = solving xs
              (stree, srest) = solving frest

stringtotree :: String -> Tree Char
stringtotree (x:xs) = Node x (fst (solving xs))

treeToString :: Tree Char -> String
treeToString (Node x ts)
          = [x] ++ intercalate "^" (map treeToString ts) ++ "^"
