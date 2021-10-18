tt :: Int -> Int
tt n | n < 0 = n * (-1) | otherwise = n
myGCD :: Int -> Int -> Int
myGCD a 0 = tt a 
myGCD a b = myGCD b (mod a b)

coprime :: Int -> Int -> Bool 
coprime a b | myGCD a b == 1 = True | otherwise = False
