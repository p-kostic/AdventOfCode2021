module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
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
partA = length . simulate 80

simulate :: Int -> [Int] -> [Int]
simulate 0 xs = xs
simulate n xs = simulate (n-1) (step xs)

step :: [Int] -> [Int]
step []     = []
step (0:xs) = 6 : 8 : step xs
step (x:xs) = (x-1) : step xs

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (\x -> countFish x !! 256)

countFish :: Int -> [Int]
countFish n = replicate (n + 1) 1 ++ zipWith (+) (countFish 6) (countFish 8)
