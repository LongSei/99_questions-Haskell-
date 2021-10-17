data ListItem a = Single a | Multiple Int a deriving (Show)
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = solving 1 x xs

solving :: (Eq a) => Int -> a -> [a] -> [ListItem a]
solving n x [] = [optimize (n, x)]
solving n x xs = if x == head xs 
                then solving (n + 1) x (tail xs)
                else [optimize (n, x)] ++ (encodeDirect xs)

optimize :: (Int, a) -> ListItem a
optimize (1, a) = Single a
optimize (n, a) = Multiple n a
