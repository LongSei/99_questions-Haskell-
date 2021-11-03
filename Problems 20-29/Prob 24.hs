import RandomPicker (pickFrom)
import Control.Monad (liftM2)
import Data.List (delete)
auxDiffSelect :: Eq a => Int -> [a] -> IO [a]
auxDiffSelect 0 _
  = return []
auxDiffSelect n xs
  = pickFrom xs >>= \x ->
    liftM2 (:) (return x) (auxDiffSelect (n - 1) (delete x xs))

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m
  | n < 0     = error "Negative number"
  | n > m     = error "Not enough numbers."
  | otherwise = auxDiffSelect n [1..m]
