-- C1: 
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

-- C2: 
dupli :: [a] -> [a]
dupli = foldl (\acc x -> acc ++ [x] ++ [x]) []
