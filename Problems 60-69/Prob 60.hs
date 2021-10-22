import Data.Maybe
import Data.List
import Control.Monad
import Data.Bits
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

hbalTree :: a -> Int -> [Tree a]
hbalTree x 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x n = [Branch x left right | (l, r) <- [(n - 1,n - 1), (n - 1,n - 2), (n - 2,n - 1)], left <- hbalTree x l, right <- hbalTree x r]

longbonacci :: Int -> Int -> Int -> Int 
longbonacci n cur acc 
  | cur >= n = acc
  | otherwise = longbonacci n nwcur (acc + 1)
              where nwcur = cur + acc

maxheight :: Int -> Int 
maxheight n = longbonacci n 1 1

minheight :: Int -> Int 
minheight n = ceiling $ logBase 2 $ fromIntegral (n + 1)
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes charc node = concatMap filterTree [mini .. maxi] 
                          where mini = minheight node
                                maxi = maxheight node
                                filterTree = filter ((node ==) . countNode) . hbalTree charc
                                countNode :: Tree a -> Int 
                                countNode Empty = 0
                                countNode (Branch _ left right) = 1 + countNode left + countNode right
