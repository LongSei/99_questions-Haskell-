{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits
import Data.Char
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
type Pos = (Int, Int)
leaf x = Branch x Empty Empty
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

layout :: Tree a -> Tree (a, Pos)
layout t = layoutAux x1 1 sep1 t
  where d = depth t
        ld = leftdepth t
        x1 = 2 ^ (d - 1) - 2 ^ (d - ld) + 1
        sep1 = 2 ^ (d - 2)
        layoutAux x y sep Empty = Empty
        layoutAux x y sep (Branch a l r) = Branch (a, (x,y)) (layoutAux (x-sep) (y+1) (sep `div` 2) l) (layoutAux (x+sep) (y+1) (sep `div` 2) r)
        depth Empty = 0
        depth (Branch a l r) = max (depth l) (depth r) + 1
        leftdepth Empty = 0
        leftdepth (Branch a l r) = leftdepth l + 1
