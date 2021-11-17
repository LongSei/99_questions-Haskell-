-- This version hasn't been optimize
import Data.Ord
import Data.Function
import Control.Monad
import Data.List
type Coord     = (Int,Int)
data Site      = Site {siteCoords :: [(Int, Int)], siteLen :: Int} deriving (Show,Eq)
data Crossword = Crossword {cwWords :: [String], cwSites :: [Site]}  deriving (Show,Eq)
string :: String
string = "ALPHA\nARES\nPOPPY\n\n  .  \n  .  \n.....\n  . .\n  . .\n    .\n"
test :: [((Integer, Integer), Char)]
test = [((3,1),'A'),((3,2),'L'),((3,3),'P'),((3,4),'H'),((3,5),'A'),((1,3),'P'),((2,3),'O'),((3,3),'P'),((4,3),'P'),((5,3),'Y'),((3,5),'A'),((4,5),'R'),((5,5),'E'),((6,5),'S')]
test' :: Crossword
test' = Crossword {cwWords = ["POPPY","ALPHA","ARES"], cwSites = [Site {siteCoords = [(1,3),(2,3),(3,3),(4,3),(5,3)], siteLen = 5},Site {siteCoords = [(3,1),(3,2),(3,3),(3,4),(3,5)], siteLen = 5},Site {siteCoords = [(5,3),(5,4),(5,5),(5,6)], siteLen = 4}]}
test'' :: [(String, Site)]
test'' = [("POPPY", Site {siteCoords = [(1,3),(2,3),(3,3),(4,3),(5,3)], siteLen = 5}), ("ALPHA", Site {siteCoords = [(3,1),(3,2),(3,3),(3,4),(3,5)], siteLen = 5}), ("ARES", Site {siteCoords = [(5,3),(5,4),(5,5),(5,6)], siteLen = 4})]
equaling :: (a -> Char) -> a -> a -> Bool
equaling = ((==) `on`)

-- Find range of character in row have '.' >= 2
extractor :: [(a, Char)] -> [[(a, Char)]]
extractor  = filter ((>1) . length) . map (filter ((=='.').snd)) . groupBy (equaling snd)

-- Map index with Char
getindex :: [[b]] -> [[((Int, Int), b)]]
getindex   = zipWith (\row -> zip [(col,row) | col <- [1..]]) [1..]

-- Make Site
makePos :: [(Coord, Char)] -> Site
makePos xs = Site {siteCoords = map fst xs, siteLen = length xs}

-- find row have extractor
finds :: [[(Coord, Char)]] -> [Site]
finds       = map makePos . concatMap extractor

-- convert the text lines from the file to the "Site" datatype, 
-- which contain the adjacent coordinates of the site and its length
toSites :: [String] -> [Site]
toSites lines = finds (getindex lines) ++ finds (transpose . getindex $ lines)

-- read the content of a file into the "Crossword" datatype
readCrossword :: String -> Crossword
readCrossword = (\(ws,ss) -> Crossword ws (toSites (drop 1 ss))) . break (""==) . lines

-- concat but other name
connect :: [[a]] -> [a]
connect = concat

-- find all the way can choose
merge :: Crossword -> [[(String, Site)]]
merge (Crossword {cwWords = x, cwSites = y}) = [zip x' y | x' <- permutations x]

convert :: Crossword -> [(Char , (Int, Int))]
convert (Crossword {cwWords = x, cwSites = y}) = cont
   where cont = connect sol
         sol = [zip x y | (x, y) <- couple]
         couple = zip x convertsite
         convertsite = [x | (Site {siteCoords = x, siteLen = number}) <- y]

-- Check satisfy len of words
checkinglen :: [(String, Site)] -> Bool
checkinglen = foldl (\acc (charc, Site {siteCoords = xy, siteLen = number}) -> length charc == number && acc) True

-- mapping character with coordinate
tolist :: Foldable t => t ([a], Site) -> [(a, (Int, Int))]
tolist = foldl (\acc (charc, Site {siteCoords = xy, siteLen = number}) -> acc ++ (connect $ [zip charc xy])) []

-- Check every situation is satisfy
checkingsatis :: [(String, Site)] -> Bool
checkingsatis xs = foldl (\acc (charc, (x, y)) -> not (exist (charc, (x, y)) (tolist xs) || acc == False)) True (tolist xs)
   where exist :: (Char, (Int, Int)) -> [(Char, (Int, Int))] -> Bool
         exist (char, (x, y)) xs = foldl (\acc (charc, (x', y')) -> if (x' == x && y' == y && charc /= char) then True else False) False xs

-- Solve the problem
solve :: Crossword -> [[(Char, (Int, Int))]]
solve xs = foldl (\acc x -> if checkingsatis x && checkinglen x then tolist x : acc else acc) [] $ merge xs
