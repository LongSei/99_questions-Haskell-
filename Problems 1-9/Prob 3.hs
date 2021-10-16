elementAt :: Int -> [a] -> a
elementAt n = head . drop (n - 1)
