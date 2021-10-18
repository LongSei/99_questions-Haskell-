import Data.List

fstfactor :: Int -> Int
fstfactor n = head $ dropWhile (\t -> n `mod` t /= 0) [2..n]

primeFactor :: Int -> [Int]
primeFactor 0 = []
primeFactor 1 = []
primeFactor n = fstfactor n : primeFactor (n `div` fstfactor n)

grouping :: Int -> [[Int]]
grouping xs = group $ sort $ primeFactor xs

primefactorsmult :: Int -> [(Int, Int)]
primefactorsmult 0 = []
primefactorsmult 1 = []
primefactorsmult xs = [(head k, length k) | k <- grouping xs]
