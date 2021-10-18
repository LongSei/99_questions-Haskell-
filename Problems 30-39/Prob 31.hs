checknot :: Bool -> [Bool] -> Bool
checknot boolen xs
  | boolen `elem` xs = False
  | otherwise = True

prime :: Int -> Bool
prime n 
  | n == 1 = False
  | n == 0 = False
  | n == 2 = True
  | otherwise = checknot True $ map (\t -> n `mod` t == 0) [2.. ceiling $ sqrt $ fromIntegral n]

isPrime :: Int -> Bool
isPrime n = prime n
