import Data.List
combinatios :: Int -> [a] -> [[a]]
combinatios 0 _ = [[]]
combinatios len xs = [y:ys | y:xs' <- tails xs, ys <- combinatios (len - 1) xs']
