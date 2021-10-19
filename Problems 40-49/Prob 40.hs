checknot :: Bool -> [Bool] -> Bool
checknot boolen xs
  | boolen `elem` xs = False
  | otherwise = True

isPrime :: Int -> Bool
isPrime n 
  | n == 1 = False
  | n == 0 = False
  | n == 2 = True
  | otherwise = checknot True $ map (\t -> n `mod` t == 0) [2.. ceiling $ sqrt $ fromIntegral n]

primeR :: Int -> Int -> [Int]
primeR left right 
    | right < left = primeR left right
    | left == right = [left | isPrime left]
    | otherwise = if isPrime left then left : primeR (left + 1) right else primeR (left + 1) right

union :: [Int] -> [Int] -> [(Int, Int)]
union xs ys = [(x, y) | x <- xs, y <- ys]

checking :: Int -> [(Int, Int)] -> (Int, Int) 
checking _ [] = (0, 0)
checking value (x:xs) = if uncurry (+) x == value then x else checking value xs

goldbach :: Int -> (Int, Int)
goldbach n = checking n $ union (primeR 2 (n - 1)) (primeR 2 (n - 1))
