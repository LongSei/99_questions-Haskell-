{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

taking :: (Eq a) => [(a, a)] -> a -> [a]
taking source x = foldl (\acc (a, b) -> b : acc) [] $ filter (\(a, b) -> x == a) source

solving :: (Eq a) => [(a, a)] -> a -> a -> a -> [a] -> [[a]]
solving source st ed nw acc
    | nw == ed = [acc]
    | otherwise = concat [solving source st ed xs (acc ++ [xs]) | xs <- filter (`notElem` acc) nx]
    where nx = taking source nw

paths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths st ed source = solving source st ed st [st]
