{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

counting :: (Eq a) => a -> [a] -> Int
counting x = foldl (\acc a -> if a == x then acc + 1 else acc) 0

taking :: (Eq a) => [(a, a)] -> a -> [a]
taking source x = foldl (\acc (a, b) -> b : acc) [] $ filter (\(a, b) -> x == a) source

solving :: (Eq a) => [(a, a)] -> a -> a -> [a] -> [[a]]
solving source st nw acc
    | counting nw acc >= 3 = []
    | nw == st && counting nw acc == 2 = [acc]
    | otherwise = concat [solving source st xs (acc ++ [xs]) | xs <- nx]
    where nx = taking source nw

cycle' :: (Eq a) => a -> [(a, a)] -> [[a]]
cycle' st source = solving source st st [st]
