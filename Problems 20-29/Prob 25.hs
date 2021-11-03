import Problem24 (auxDiffSelect)
rndPermu :: Eq a => [a] -> IO [a]
rndPermu xs = auxDiffSelect (length xs) xs
