{-# LANGUAGE TypeApplications #-}
import Data.List
import Control.Monad
import Data.Bits

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

depthleft :: Tree a -> Int
depthleft Empty = 0
depthleft (Branch x left right) = 1 + depthleft left

solving :: Tree a -> Int -> Int ->  Tree (a, Pos)
solving (Branch x Empty Empty) level dleft = Branch (x, (max dleft (depthleft (Branch x Empty Empty)), level)) Empty Empty
solving (Branch x Empty right) level dleft = Branch (x, (max dleft (depthleft (Branch x Empty right)), level)) Empty (solving right (level + 1) (dleft + 1))
solving (Branch x left Empty) level dleft = Branch (x, (max dleft (depthleft (Branch x left Empty)), level)) (solving left (level + 1) (dleft - 1)) Empty
solving (Branch x left right) level dleft = Branch (x, (max dleft (depthleft (Branch x left right)), level)) (solving left (level + 1) (dleft - 1)) (solving right (level + 1) (dleft + 1))

layout :: Tree a -> Tree (a, Pos)
layout (Branch x Empty Empty) = Branch (x, (depthleft (Branch x Empty Empty) + 1, 1)) Empty Empty
layout (Branch x Empty right) = Branch (x, (depthleft (Branch x Empty right) + 1, 1)) Empty (solving right 2 (depthleft (Branch x Empty right) + 3))
layout (Branch x left Empty) = Branch (x, (depthleft (Branch x left Empty) + 1, 1)) (solving left 2 (depthleft (Branch x left Empty) - 1)) Empty
layout (Branch x left right) = Branch (x, (depthleft (Branch x left right) + 1, 1)) (solving left 2 (depthleft (Branch x left right) - 1)) (solving right 2 (depthleft (Branch x left right) + 3))
