{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

type Node = Int
type Edge = (Node,Node)
type Graph = ([Node],[Edge])
type Stack = [Int]

listnx :: Int -> [(Int, Int)] -> [Int] -> [Int]
listnx charc vs acc
    | null vs = acc
    | x == charc = listnx charc xs (y:acc)
    | y == charc = listnx charc xs (x:acc)
    | otherwise = listnx charc xs acc
    where (x, y) = head vs
          xs = tail vs

depthfirst :: ([Node], [Edge]) -> Int -> [Int] -> Stack -> [Int]
depthfirst (node, edge) nw take stack
    | null remain && null stack = take
    | null remain = if null cl then take else depthfirst (node, edge) (head cl) take cl
    | otherwise = depthfirst (node, edge) (head remain) (take ++ [head remain]) (head remain : stack)
    where nx point = dropWhile (`elem` take) $ listnx point edge []
          remain = nx nw
          cl = dropWhile (null . nx) stack
