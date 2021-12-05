module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy` endOfLine

parseLine :: Parser Line
parseLine = do firstX <- decimal 
               char ','
               firstY <- decimal
               string " -> "
               sndX <- decimal
               char ','
               sndY <- decimal
               return ((firstX, firstY), (sndX, sndY))

------------ TYPES ------------

type Point = (Int, Int)
type Line = (Point, Point)
type Input = [Line]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA ls = intersectingPoints (foldMap getPoints (filter isStraight ls))
         where isStraight ((a, b), (c, d)) = a == c || b == d

intersectingPoints :: [Point] -> Int
intersectingPoints ls = Map.size (Map.filter (>1) (counts ls))

counts :: [Point] -> Map Point Int
counts = foldr (flip (Map.insertWith (+)) 1) Map.empty

getPoints :: Line -> [Point]
getPoints ((a, b), (c, d)) = zip (range a c) (range b d)
                        where range start end = [start, start + signum (end - start) .. end]
------------ PART B ------------
partB :: Input -> OutputB
partB ls = intersectingPoints (foldMap getPoints ls)
