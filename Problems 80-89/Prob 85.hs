{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.Ord
import Data.List

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)

graphG1 = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 8), (3, 5), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8)]
graphH1 = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2), (1, 4), (1, 5), (6, 2), (6, 5), (6, 7), (8, 4), (8, 5), (8, 7), (3, 2), (3, 4), (3, 7)]

graph_to_adjacency :: (Eq a) => Graph a -> Adjacency a
graph_to_adjacency (Graph [] _) = Adj []
graph_to_adjacency (Graph (x:xs) ys) = Adj ((x, ys >>= f) : tails)
    where f (a, b) | a == x = [b] | b == x = [a] | otherwise = []
          Adj tails = graph_to_adjacency (Graph xs ys)

sortbyfunc :: (Foldable t, Ord b) => (a -> b) -> t a -> [a]
sortbyfunc f = foldr (\x xs -> let (lt, gt) = break ((<) (f x) . f) xs in lt ++ [x] ++ gt) []
-- Split list into a pair 

found :: Eq b1 => [(b1, b2)] -> (b1, b3) -> (b1, b2)
found a x = let (xs, ys) = break ((==) (fst x) . fst) a in head ys
-- Found element with the same fst

convert :: (Eq a) => Graph a -> String
convert g = minimum $ map f $ permutations [1 .. length a]
   where
      Adj a = graph_to_adjacency g -- ADJLIST
      v = map fst a -- Node in graph
      f p = let n = zip v p -- n is mapping list
            in show [(snd x, sortbyfunc id
              $ map (\x -> snd $ head $ snd $ break ((==) x . fst) n) -- node coverted with x
              $ snd $ found a x) -- list adjlist with choosing node
              | x <- sortbyfunc snd n] -- sort by snd value

iso :: (Eq a) => Graph a -> Graph a -> Bool 
iso xs ys = convert xs == convert ys
