module Days.Day11 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (digitToInt <$> digit) `sepBy` endOfLine

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA xs = simulate (U.mapFromNestedLists xs) 0 101 0 

simulate :: Map (Int, Int) Int -> Int -> Int -> Int -> OutputA
simulate grid round l n | round == l  = n
                        | otherwise   = simulate grid' (round + 1) l (n + flashes)
                        where grid'   = flash (Map.map (1+) grid) []
                              flashes = length (filter (0==) (Map.elems grid))

flash :: Map (Int, Int) Int -> [(Int, Int)] -> Map (Int, Int) Int
flash grid flashed | null fs   = grid
                   | otherwise = flash grid' fs'
                   where grid' = setToZero fs' (Map.mapWithKey (\key value -> value + nFlashed key grid) grid)
                         fs    = getFlashed grid
                         fs'   = fs ++ flashed

-- (!?) :: Ord k => Map k a -> k -> Maybe a
nFlashed :: (Int, Int) -> Map (Int, Int) Int -> Int
nFlashed pos grid = length (filter (>9) (mapMaybe (grid Map.!?) (neighbors pos))) 

getFlashed :: Map (Int, Int) Int -> [(Int, Int)]
getFlashed grid = Map.keys (Map.filterWithKey (\_ value -> value > 9) grid)

setToZero :: [(Int, Int)] -> Map (Int, Int) Int -> Map (Int, Int) Int
setToZero xs = Map.mapWithKey (\key value -> if key `elem` xs then 0 else value)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1)]

------------ PART B ------------
partB :: Input -> OutputB
partB xs = whenSynchronized (U.mapFromNestedLists xs) 0

whenSynchronized :: Map (Int, Int) Int -> Int -> OutputB
whenSynchronized grid round | all (0==) (Map.elems grid) = round 
                            | otherwise                  = whenSynchronized (flash (Map.map (1+) grid) []) (round + 1)
