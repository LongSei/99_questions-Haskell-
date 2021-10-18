tt :: Int -> Int
tt n | n < 0 = n * (-1) | otherwise = n
myGCD :: Int -> Int -> Int
myGCD a 0 = tt a 
myGCD a b = myGCD b (mod a b)
