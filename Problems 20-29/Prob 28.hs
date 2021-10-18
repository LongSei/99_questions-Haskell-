lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = smaller ++ [x] ++ bigger
            where smaller = lsort [a | a <- xs, length a <= length x]
                  bigger = lsort [a | a <- xs, length a > length x]

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort (x:xs) = 
            let smaller = lfsort [a | a <- xs, cnt (length a) < cnt (length x)]
                bigger = lfsort [a | a <- xs, cnt (length a) >= cnt (length x)]
            in smaller ++ [x] ++ bigger where
               cnt :: Int -> Int
               cnt len = foldl (\acc k -> if length k == len then acc + 1 else acc) 0 (x:xs)
