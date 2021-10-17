repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) timer = (replicate timer x) ++ (repli xs timer)
