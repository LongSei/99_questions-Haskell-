{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

data ListItem a = Single a | Multiple Int a deriving (Show)
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = solving 1 x xs

solving :: (Eq a) => Int -> a -> [a] -> [ListItem a]
solving n x [] = if n == 1 then [Single x] else [Multiple n x]
solving n x xs = if x == head xs then solving (n + 1) x (tail xs) else optimize (n, x) : encodeDirect xs
    where optimize :: (Int, a) -> ListItem a
          optimize (1, a) = Single a
          optimize (n, a) = Multiple n a
