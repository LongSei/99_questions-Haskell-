-- C1: 
repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) timer = (replicate timer x) ++ (repli xs timer)

-- C2: 
repli :: [a] -> Int -> [a]
repli xs timer = foldl (\acc x -> acc ++ replicate timer x) [] xs
