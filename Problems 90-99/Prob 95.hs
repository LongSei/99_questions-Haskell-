{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

fullWords :: Int -> String
fullWords n = intercalate "-" [name k | k <- show n]

name :: Char -> String
name charc 
    | charc == '0' = "zero"
    | charc == '1' = "one"
    | charc == '2' = "two"
    | charc == '3' = "three"
    | charc == '4' = "four"
    | charc == '5' = "five"
    | charc == '6' = "six"
    | charc == '7' = "seven"
    | charc == '8' = "eight"
    | charc == '9' = "nine"
    | otherwise   = error "Not a Num"
