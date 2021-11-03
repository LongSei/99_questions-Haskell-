{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

type Node = Int
type Edge = (Node,Node)
type Graph = ([Node],[Edge])
type Stack = [Int]

tree1 :: ([Int], [(Int, Int)])
tree1 = ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
tree2 :: ([Int], [(Int, Int)])
tree2 = ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])

listnx :: Int -> [(Int, Int)] -> [Int] -> [Int]
listnx charc vs acc
    | null vs = acc
    | x == charc = listnx charc xs (y:acc)
    | y == charc = listnx charc xs (x:acc)
    | otherwise = listnx charc xs acc
    where (x, y) = head vs
          xs = tail vs

sameele :: [Int] -> [Int] -> Bool
sameele xs ys = foldl (\acc x -> x `elem` ys) False xs

pushin :: [Int] -> [Int] -> [Int]
pushin xs = foldl (\acc x -> if x `elem` acc then acc else x : acc) xs

solving :: Graph -> Int -> [Int] -> [Int] -> Stack -> Bool
solving (node, edge) al left right remain
  | null remain = if length left + length right == al then True else solving (remainnode, edge) al (left ++ listnx (head remainnode) edge []) (head remainnode : right) (listnx (head remainnode) edge [])
  | position == 0 = if sameele opposite left then False else solving (node, edge) al (pushin [checking] left) (pushin opposite right) nwstack
  | position == 1 = if sameele opposite right then False else solving (node, edge) al (pushin opposite left) (pushin [checking] right) nwstack
  where checking = head remain
        nw = pushin (tail remain) nwstack
        nwstack = foldl (\acc x -> if elem x left || elem x right || elem x remain then acc else x : acc) (tail remain) opposite
        remainnode = foldl (\acc x -> if elem x left || elem x right then acc else x : acc) [] node
        position | checking `elem` left = 0 | checking `elem` right = 1 | otherwise = 2
        opposite = listnx checking edge []

bipartite :: Graph -> Bool
bipartite (node, edge) = solving (node, edge) (length node) [1] (listnx 1 edge []) (listnx 1 edge [])
