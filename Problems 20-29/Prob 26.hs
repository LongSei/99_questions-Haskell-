-- Solution 1 --
import Data.List
combinatios :: Int -> [a] -> [[a]]
combinatios 0 _ = [[]]
combinatios len xs = [y:ys | y:xs' <- tails xs, ys <- combinatios (len - 1) xs']

-- Solution 2 --
combination :: Int -> [a] -> [[a]]
combination 0 xs = []
combination 1 xs = map (:[]) xs
combination n [] = []
combination n (x:xs) = (map (x:) $ combination (n - 1) xs) ++ combination n xs
