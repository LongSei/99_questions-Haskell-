{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits
data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)
data Friendly a = Edge [(a, a)] deriving (Show, Eq)

graph_check = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
adjacency_check = Adj [('b',"cf"),('c',"bf"),('d',""),('f',"bck"),('g',"h"),('h',"g"),('k',"f")]
friendly_check = Edge [('b','c'),('b','f'),('c','f'),('f','k'),('g','h'),('d','d')]

graph_to_adjacency :: (Eq a) => Graph a -> Adjacency a
graph_to_adjacency (Graph [] _) = Adj []
graph_to_adjacency (Graph (x:xs) ys) = Adj ((x, ys >>= f) : tails)
    where f (a, b) | a == x = [b] | b == x = [a] | otherwise = []
          Adj tails = graph_to_adjacency (Graph xs ys)

graph_to_friendly :: (Eq a) => Graph a -> Friendly a
graph_to_friendly (Graph [] _) = Edge []
graph_to_friendly (Graph xs ys) = Edge (ys ++ zip only only)
    where only = filter (\x -> all (\(a, b) -> x /= a && x /= b) ys) xs

adjacency_to_graph :: (Eq a) => Adjacency a -> Graph a
adjacency_to_graph (Adj []) = Graph [] []
adjacency_to_graph (Adj (x:vs)) = Graph (fst x :xs) ((snd x >>= f) ++ ys)
    where f k = [(k, fst x) | not ((fst x, k) `elem` ys || (k, fst x) `elem` ys)]
          Graph xs ys = adjacency_to_graph (Adj vs)

adjacency_to_friendly :: (Eq a) => Adjacency a -> Friendly a
adjacency_to_friendly = graph_to_friendly . adjacency_to_graph

friendly_to_graph :: (Eq a) => Friendly a -> Graph a
friendly_to_graph (Edge []) = Graph [] []
friendly_to_graph (Edge vs) = Graph xs ys
    where xs = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) [] $ concat $ map (\(a, b) -> [a, b]) vs
          ys = foldl (\acc (a, b) -> if a == b then acc else acc ++ [(a, b)]) [] vs

friendly_to_adjacency :: (Eq a) => Friendly a -> Adjacency a 
friendly_to_adjacency = graph_to_adjacency . friendly_to_graph
