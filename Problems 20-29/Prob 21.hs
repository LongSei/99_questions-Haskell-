insertAt :: a -> [a] -> Int -> [a]
insertAt charc xs indx = (take (indx - 1) xs) ++ [charc] ++ (drop (indx - 1) xs) 
