{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

counting :: (Eq a) => [(a, a)] -> Int
counting xs = length $ foldl (\acc x -> if x `elem` acc then acc else x:acc) [] $ concatMap (\(a, b) -> [a, b]) xs

k4 = Graph ['a', 'b', 'c', 'd'] [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]
k3 = Graph ['a', 'b', 'c'] [('a', 'b'), ('b', 'c')]

solve :: (Eq a) => [(a, a)] -> [(a, a)] -> Int -> [[(a, a)]]
solve (x:remain) take totalnode
    | remain == [] && counting take /= totalnode = []
    | counting take == totalnode = [take]
    | remain == [] && counting (x:take) == totalnode = [x:take]
    | otherwise = solve remain (x:take) totalnode ++ solve remain take totalnode 

spanningtree :: (Eq a) => Graph a -> [Graph a] 
spanningtree (Graph node edge) = [Graph node xs | xs <- solve edge [] (length node)]
