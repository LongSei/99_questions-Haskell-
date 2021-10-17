slice :: [a] -> Int -> Int -> [a]
slice xs indx1 indx2 = solve xs (indx1 - 1) ((length xs) - indx2)

solve :: [a] -> Int -> Int -> [a]
solve xs indx1 indx2 = reverse (drop indx2  (reverse (drop indx1 xs)))
