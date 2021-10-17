split :: [a] -> Int -> ([a], [a])
split xs timer = ((take timer xs), (drop timer xs))
