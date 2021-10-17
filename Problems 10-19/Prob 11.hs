data ListItem a = Single a | Multiple Int a deriving (Show)

check (1, x) = Single x
check (n, x) = Multiple n x

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (length $ x : takeWhile (==x) xs, x) : encode (dropWhile (==x) xs)

encodeVer2 :: Eq a => [a] -> [ListItem a]
encodeVer2 = map check . encode
