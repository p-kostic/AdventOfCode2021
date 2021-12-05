module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.List (transpose)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do drawnNumbers <- decimal `sepBy` char ','                 
                 skipSpace
                 let block = initState <$> (option () skipH *> decimal `sepBy1'` many1' skipH) `sepBy1'` endOfLine
                 boards <- block `sepBy` skipSpace
                 pure (Bingo drawnNumbers boards) 

skipH :: Parser ()
skipH = skip isHorizontalSpace

initState :: [[Int]] -> [[(Int, Bool)]]
initState = map (map (, False))

-- skipSpaceNoNewline :: Parser ()
-- skipSpaceNoNewline = skipWhile (\x -> isSpace x && not (isEndOfLine x))

------------ TYPES ------------

data Bingo = Bingo {
    drawnNumbers :: [Int],
    boards       :: [Board]
} deriving Show

type Board = [[(Int, Bool)]]

type Input = Bingo

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- mark all boards until any has bingo. If so, stop, get that one out of the board list and calculate its score.
partA :: Input -> OutputA
partA (Bingo (x:xs) bs) | any hasBingo nextRound = calculateScore x (getBingoBoard nextRound)
                        | otherwise              = partA (Bingo xs nextRound)
                        where nextRound = map (markBoard x) bs

hasBingo :: Board -> Bool
hasBingo board | any (all snd) board             = True
               | any (all snd) (transpose board) = True
               | otherwise                       = False

-- Potentially TODO: Better way to get the board from hasBingo maybe?
getBingoBoard :: [Board] -> Board
getBingoBoard bs = head $ filter hasBingo bs

markBoard :: Int -> Board -> Board
markBoard n = map (markRow n)

markRow :: Int -> [(Int, Bool)] -> [(Int, Bool)]
markRow n [(n', b)]   | n == n'   = [(n', True)]
                      | otherwise = [(n', b)]                
markRow n ((n',b):xs) | n == n'   = (n', True) : markRow n xs
                      | otherwise = (n', b)    : markRow n xs

calculateScore :: Int -> Board -> Int
calculateScore n board = n * sum (getUnmarkedNumbers (concat board))

getUnmarkedNumbers :: [(Int, Bool)] -> [Int]
getUnmarkedNumbers [(x,False)]    = [x]
getUnmarkedNumbers [(x,True)]     = []
getUnmarkedNumbers ((x,False):xs) = x : getUnmarkedNumbers xs
getUnmarkedNumbers ((x,True):xs)  = getUnmarkedNumbers xs

------------ PART B ------------
-- Recursively play until there's only one board that can possibly win, and play that one out in PartA
partB :: Input -> OutputB
partB (Bingo (x:xs) bs) | length getLast == 1 = partA (Bingo xs getLast)
                        | otherwise           = partB (Bingo xs nextRound)
                        where nextRound       = map (markBoard x) bs
                              getLast = filter (not . hasBingo) nextRound 
