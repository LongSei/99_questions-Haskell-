{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.Ord
import Data.List

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)

graph :: Graph Char
graph = Graph ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]

graph_to_adjacency :: (Eq a) => Graph a -> Adjacency a
graph_to_adjacency (Graph [] _) = Adj []
graph_to_adjacency (Graph (x:xs) ys) = Adj ((x, ys >>= f) : tails)
    where f (a, b) | a == x = [b]
                   | b == x = [a]
                   | otherwise = []
          Adj tails = graph_to_adjacency (Graph xs ys)

findvalue :: Eq b => Adjacency b -> b -> (b, [b])
findvalue (Adj xs) x = head $ snd $ break ((==) x . fst) xs

degree :: Eq b => Adjacency b -> b -> Int
degree (Adj xs) x = length $ snd $ findvalue (Adj xs) x

sortbydegree :: Ord a => Adjacency a -> [a]
sortbydegree (Adj xs) = reverse checking
      where nwlist = foldl (\acc (x, ys) -> (length ys, x) : acc) [] xs
            checking = map snd $ sort nwlist

kcolor :: (Eq a, Ord a) => Graph a -> [(a, Int)]
kcolor graph' = choosing aftersort [] 1
      where adjlistofgraph = graph_to_adjacency graph'
            aftersort = foldl (\acc x -> acc ++ [findvalue adjlistofgraph x]) [] $ sortbydegree adjlistofgraph
            choosing [] have_color _ = have_color
            choosing remain have_color number = let new_color = color remain have_color number
                                                in choosing [(x, adjlistofx) | (x, adjlistofx) <- remain, (x,number) `notElem` new_color] new_color (number + 1)
            color [] have_color number = have_color
            color ((node, adj):xs) have_color number = if any (\x -> (x, number) `elem` have_color) adj
                                                  then color xs have_color number
                                                  else color xs ((node, number) : have_color) number
