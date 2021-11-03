{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits
import Data.Char
defeat try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs
isok try qs = not (try `elem` qs || defeat try qs)
queen :: Int -> [[Int]]
queen n = map reverse $ solving n
  where solving 0 = [[]]
        solving k = [q:qs | qs <- solving (k-1), q <- [1 .. n], isok q qs]
