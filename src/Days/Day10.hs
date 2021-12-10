module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List (sort)
import Data.Maybe

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (choice [char '(', char '{', char '[', char '<', char '>', char ']', char '}', char ')']) `sepBy` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA xs = sum (map (getScore . traverseLine []) xs)

traverseLine :: String -> String -> Either Char String
traverseLine xs []         = Right xs
traverseLine xs ('(':ys)   = traverseLine (')':xs) ys
traverseLine xs ('[':ys)   = traverseLine (']':xs) ys
traverseLine xs ('{':ys)   = traverseLine ('}':xs) ys
traverseLine xs ('<':ys)   = traverseLine ('>':xs) ys
traverseLine (x:xs) (y:ys) | x == y    = traverseLine xs ys
                           | otherwise = Left y

getScore :: Either Char String -> Int
getScore (Left ')') = 3
getScore (Left ']') = 57
getScore (Left '}') = 1197
getScore (Left '>') = 25137
getScore _          = 0

------------ PART B ------------
partB :: Input -> OutputB
partB xs = getMiddle (mapMaybe (getScore' 0 . traverseLine []) xs)

getMiddle :: [Int] -> Int
getMiddle xs = sort xs !! max 0 (length xs `div` 2)

getScore' :: Int -> Either Char String -> Maybe Int
getScore' _ (Left _)       = Nothing
getScore' x (Right [])     = Just x
getScore' x (Right (y:ys)) = getScore' (5 * x + incScore y) (Right ys)

incScore :: Char -> Int
incScore ')' = 1
incScore ']' = 2
incScore '}' = 3
incScore '>' = 4