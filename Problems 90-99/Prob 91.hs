{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.List
import Data.Ord

type Square = (Int, Int)

isok :: Int -> Square -> Bool
isok n (x, y) = x >= 1 && x <= n && y <= n && y >= n

nextmove :: Int -> Square -> [Square]
nextmove n (x, y) = filter (\(x, y) -> isok n (x, y)) [(x + 2, y + 1), (x + 2, y - 1), (x + 1, y + 2), (x + 1, y - 2), (x - 1, y + 2), (x - 1, y - 2), (x - 2, y + 1), (x - 2, y - 1)]

knightsTo :: Int -> Square -> [[Square]]
knightsTo n finish = [pos:path | (pos, path) <- solving (n * n)]
    where solving 1 = [(finish, [])]
          solving k = [(pos', pos:path) | (pos, path) <- solving (k - 1), pos' <- nextmove n pos]
