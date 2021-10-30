{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq)
graph = Graph [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

counting :: (Eq a) => [(a, a, Int)] -> Int
counting xs = length $ foldl (\acc x -> if x `elem` acc then acc else x:acc) [] $ concatMap (\(a, b, c) -> [a, b]) xs

solve :: (Eq a) => [(a, a, Int)] -> [(a, a, Int)] -> Int -> [[(a, a, Int)]]
solve (x:remain) take totalnode
    | null remain && counting take /= totalnode = []
    | counting take == totalnode = [take]
    | null remain && counting (x:take) == totalnode = [x:take]
    | otherwise = solve remain (x:take) totalnode ++ solve remain take totalnode

isconnect :: (Eq a) => Graph a -> [a] -> Bool
isconnect (Graph k vs) take
    | null vs = if length take == length k then True else False
    | null take = isconnect (Graph k xs) (a : b : take)
    | elem a take && elem b take = isconnect (Graph k xs) take
    | elem a take = isconnect (Graph k xs) (b : take)
    | elem b take = isconnect (Graph k xs) (a : take)
    | exist a xs == False && exist b xs == False = False
    | takemore take xs == False = False 
    | otherwise = isconnect (Graph k (tail vs ++ [head vs])) take 
    where (a, b, c) = head vs
          xs = tail vs
          takemore :: (Eq a) => [a] -> [(a, a, Int)] -> Bool 
          takemore ks kss = foldl (\acc i -> if (exist i kss) || acc == True then True else False) False ks
          exist :: (Eq a) => a -> [(a, a, Int)] -> Bool 
          exist k ks = foldl (\acc (i, o, p) -> if (i == k || o == k) || acc == True then True else False) False ks

spanningtree :: (Eq a) => Graph a -> [Graph a]
spanningtree (Graph node edge) = setting xs 
    where xs = [Graph node xs | xs <- solve edge [] (length node)]
          setting :: (Eq a) => [Graph a] -> [Graph a]
          setting k = foldl (\acc x -> if isconnect x [] then x:acc else acc) [] k 
