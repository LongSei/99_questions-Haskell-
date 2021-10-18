fstfactor :: Int -> Int
fstfactor n = head $ dropWhile (\t -> n `mod` t /= 0) [2..n]

primeFactor :: Int -> [Int]
primeFactor 0 = []
primeFactor 1 = []
primeFactor n = (fstfactor n) : (primeFactor (n `div` fstfactor n))
