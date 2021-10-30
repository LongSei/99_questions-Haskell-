{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits
import Data.Char

identifier :: String -> Bool
identifier [] = False
identifier (x : xs) = isAlpha x && rest xs
  where rest remain = case remain of
          [] -> True
          [c] -> isAlphaNum c 
          '-' : c : cs  -> isAlpha c && rest cs
          c : cs -> rest cs
