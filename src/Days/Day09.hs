module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (take)
import Data.Char (digitToInt)
import Data.Ord
import Data.List (sortOn)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (map digitToInt <$> many1 digit) `sepBy` endOfLine

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

-- NOTE for A and B: Didn't have time to make this any more efficient or readable.
--                   Additionally, this was awful to debug..
------------ PART A ------------
partA :: Input -> OutputA
partA xs = sum (map (\x -> 1 + getPos xs x) (lowPoint (0, 0) xs))

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

lowPoint :: (Int, Int) -> [[Int]] -> [(Int, Int)]
lowPoint (x,y) xs | x < length (head xs) = if isLowPoint (x,y) xs then (x,y) : lowPoint (x+1, y) xs else lowPoint (x+1, y) xs
                  | otherwise            = if y < length xs       then lowPoint(0, y+1) xs          else []

isLowPoint :: (Int, Int) -> [[Int]] -> Bool
isLowPoint (x,y) xs = all ((> getPos xs (x,y)) . getPos xs) (neighbors (x,y))

getPos :: [[Int]] -> (Int, Int) -> Int
getPos xs (x,y) | x >= 0 && x < (length . head) xs && y >= 0 && y < length xs = xs !! y !! x
                | otherwise                                                   = 9

------------ PART B ------------
partB :: Input -> OutputB
partB xs = partB' (map (findBasin xs []) (lowPoint (0, 0) xs))
         where partB' = product . take 3 . sortOn Data.Ord.Down . map (sum . map length)

findBasin :: [[Int]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
findBasin xs s (x,y) | getPos xs (x,y) < 9 && notElem (x,y) s = findBasin xs d' d
                     | otherwise                              = s
                     where [a,b,c,d] = neighbors (x,y)
                           b'        = findBasin xs ((x,y):s) a
                           c'        = findBasin xs b'        b
                           d'        = findBasin xs c'        c
