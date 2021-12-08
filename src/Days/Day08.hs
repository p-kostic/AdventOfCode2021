module Days.Day08 (runDay) where

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

parseLine :: Parser ([String], [String])
parseLine = (,) <$> many1 parseSegment `sepBy` " " <* " | " <*> many1 parseSegment `sepBy` " "

parseSegment :: Parser Char                   
parseSegment = choice [char 'a',char 'b',char 'c',char 'd',char 'e',char 'f',char 'g']

------------ TYPES ------------
type Input = [([String], [String])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- For reference, from problem description:
-- 0 : 6 segments
-- 1 : 2 segments (unique)
-- 2 : 5 segments
-- 3 : 5 segments
-- 4 : 4 segments (unique)
-- 5 : 5 segments
-- 7 : 3 segments (unique)
-- 8 : 7 segments (unique)
-- 9 : 6 segments

partA :: Input -> OutputA
partA xs = length (concatMap (filter (`elem` [1, 4, 7, 8])) (getAllDigits xs (map findCorrectInPerms xs)))

segmentMap :: Map.Map String Int
segmentMap = Map.fromList [("ab",1), ("acdfg",2),("abcdf",3),("abef",4),("bcdef",5),("bcdefg",6),("abd",7),("abcdefg",8),("abcdef",9),("abcdeg",0)]

getAllDigits :: [([String], [String])] -> [Map.Map Char Char] -> [[Int]]
getAllDigits = zipWith getDigits

getDigits :: ([String], [String]) -> Map.Map Char Char -> [Int]
getDigits (_, out) correct = map ((segmentMap Map.!) . sort . map (correct Map.!)) out

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc x -> acc * 10 + x) 0

findCorrectInPerms :: ([String], [String]) -> Map.Map Char Char
findCorrectInPerms (xs, out) = head (filter (isMatch (xs, out)) allPermutations)

allPermutations :: [Map.Map Char Char]
allPermutations = map (Map.fromList . zip "abcdefg") (permutations "abcdefg")

isMatch :: ([String], [String]) -> Map.Map Char Char -> Bool
isMatch (xs, _) perm = all (\x -> sort x `elem` Map.keys segmentMap) (permuted perm xs)

permuted :: Map.Map Char Char -> [String] -> [String]
permuted perm = map (map (perm Map.!))

------------ PART B ------------
partB :: Input -> OutputB
partB xs = sum (map digitsToInt (getAllDigits xs (map findCorrectInPerms xs)))
