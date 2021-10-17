rotate :: [a] -> Int -> [a]
rotate xs indx = if indx >= 0
                then (drop indx xs) ++ (take indx xs)
                else rotate xs (length xs + indx)
