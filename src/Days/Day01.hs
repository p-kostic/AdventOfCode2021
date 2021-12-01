module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
-- Unfamiliar with this template's parsing package, I'll keep some hoogle references here for future Days
-- endOfLine :: Parser ()                             https://hackage.haskell.org/package/attoparsec-0.14.2/docs/Data-Attoparsec-ByteString-Char8.html#v:endOfLine
-- decimal   :: Integral a => Parser a                https://hackage.haskell.org/package/attoparsec-0.14.2/docs/Data-Attoparsec-ByteString-Char8.html#v:decimal
-- sepBy     :: Alternative f => f a -> f s -> f [a]  https://hackage.haskell.org/package/attoparsec-0.14.2/docs/Data-Attoparsec-ByteString.html#v:sepBy
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Use zip to make tuples of sliding window
partA :: Input -> OutputA
partA xs = length (filter (uncurry (<)) (zip xs (tail xs)))

------------ PART B ------------
-- Same sliding principle as PartA, but with zip3, summing tuple values as input to PartA
partB :: Input -> OutputB
partB xs = partA (map (\(a, b, c) -> a + b + c) (zip3 xs xs' (tail xs')))
         where xs' = tail xs