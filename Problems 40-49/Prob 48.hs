{-# LANGUAGE TypeApplications #-}
import Control.Monad
import Data.Bits
import Data.List
import Data.Ord

and',or',nor',nand',xor',impl',equ' :: Bool -> Bool -> Bool
not' :: Bool -> Bool
not' True = False
not' False = True

and' x y | x == y && y = True | otherwise = False
or' x y | x || y = True | otherwise = False
nor' x y = not' $ or' x y
nand' x y = not' $ and' x y
xor' x y | x == y = False | otherwise = True
impl' x = or' (not' x)
equ' x y | x == y = True | otherwise = False

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- xs, b <- ys] 
          where xs = [True, False]
                ys = [True, False]

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++  " " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "
