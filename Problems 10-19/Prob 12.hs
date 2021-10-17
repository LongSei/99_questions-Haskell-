data ListItem a = Single a | Multiple Int a deriving (Show)
decodeModified :: [ListItem a] -> [a] 
decodeModified = foldl (\acc e -> case e of Single x -> acc ++ [x]; Multiple n x -> acc ++ (replicate n x)) []

