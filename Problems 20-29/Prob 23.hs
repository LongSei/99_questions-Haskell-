import RandomPick (pickFrom)
import Control.Monad (liftM2)
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
    | n < 0     = error "Negative value!!"
    | n == 0    = return []
    | otherwise = liftM2 (:) (pickFrom xs) (rndSelect xs (n - 1))
