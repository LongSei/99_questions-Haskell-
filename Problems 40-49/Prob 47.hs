infixl 4 `or'`
infixl 6 `and'`

and',or',nor',nand',xor',impl',equ' :: Bool -> Bool -> Bool
not' :: Bool -> Bool
not' True = False
not' False = True

and' x y | x == y && y = True | otherwise = False
or' x y | x || y = True | otherwise = False
nor' x y = not' $ or' x y
nand' x y = not' $ and' x y
xor' x y | x == y = False | otherwise = True
impl' x = or' (not' x)
equ' x y | x == y = True | otherwise = False

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- xs, b <- ys] 
          where xs = [True, False]
                ys = [True, False]
