module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.List
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA xs = sum $ getFuelCost (median xs) <$> xs  

-- Median function was borrowed and slightly modified to remove 'Fractional a =>' from https://stackoverflow.com/a/51092619/4516065
medianFromSorted :: Num a => Integral a => [a] -> a
medianFromSorted []      = 0
medianFromSorted [a]     = a
medianFromSorted [a,b]   = floor (fromIntegral (a + b) / 2)
medianFromSorted [a,b,c] = b
medianFromSorted (a:xs)  = medianFromSorted (init xs)

median :: Ord a => Num a => Integral a => [a] -> a
median = medianFromSorted . sort

getFuelCost :: Int -> Int -> Int 
getFuelCost x y = abs (x - y)

------------ PART B ------------
partB :: Input -> OutputB
partB xs = sum (getFuelCost' (mean xs) <$> xs)

getFuelCost' :: Int -> Int -> Int
getFuelCost' x y = d * (d+1) `div` 2
                 where d = abs (x - y)

mean :: Num a => Integral a => [a] -> a
mean xs = floor (fromIntegral (sum xs) / fromIntegral (length xs))

