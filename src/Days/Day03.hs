module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Control.Applicative ((<|>))
import Data.List
import Data.Ord
import Data.Char (digitToInt)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =  many' (True <$ char '1' <|> False <$ char '0') `sepBy` endOfLine

------------ TYPES ------------

type Input = [[Bool]]

data Rating = O2 | CO2 deriving (Eq, Show)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA xs = toDec gamma * toDec(invert gamma)
         where gamma = map (gammaPart (0,0)) (transpose xs) 

gammaPart :: (Int, Int) -> [Bool] -> Char
gammaPart (g,e) []         | g > e     = '1'
                           | otherwise = '0'
gammaPart (g,e) (True:xs)  = gammaPart (g+1, e) xs
gammaPart (g,e) (False:xs) = gammaPart (g, e+1) xs

invert :: String -> String
invert = map f
       where f '0' = '1'
             f '1' = '0'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

toDec' :: Int -> [Bool] -> Int
toDec' = foldl' (\acc x -> acc * 2 + fromEnum x)

------------ PART B ------------
partB :: Input -> OutputB
partB xs = calculateRating' 0 xs CO2 * calculateRating' 0 xs O2

calculateRating' :: Int -> [[Bool]] -> Rating -> Int
calculateRating' r xs rtype | length xs == 1 = toDec' r (head xs)
                            | otherwise      = calculateRating' (r * 2 + fromEnum (getFromCriteria xs rtype)) (tail <$> filter ((==getFromCriteria xs rtype) . head) xs) rtype

getFromCriteria :: [[Bool]] -> Rating -> Bool
getFromCriteria xs = applyFromOrd (head <$> xs)                        

applyFromOrd :: [Bool] -> Rating -> Bool
applyFromOrd xs rtype = (moreTrues xs, rtype) `elem` [(GT, O2), (LT, CO2), (EQ, O2)]

moreTrues :: [Bool] -> Ordering
moreTrues xs = compare (length (filter id xs)) (length (filter not xs))
