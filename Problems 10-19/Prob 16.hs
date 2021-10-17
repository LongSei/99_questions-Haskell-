dropEvery :: [a] -> Int -> [a]
dropEvery xs timer = solve xs timer timer

solve :: [a] -> Int -> Int -> [a]
solve [] _ _ = []
solve (x : xs) need remain = if remain == 1
                                then solve xs need need
                                else [x] ++ solve xs need (remain - 1)
