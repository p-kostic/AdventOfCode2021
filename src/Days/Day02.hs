module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Text (Text)
import Data.Attoparsec.Text
import Data.Attoparsec.ByteString.Char8 (isSpace)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' $ do text <- takeTill isSpace
                         space
                         amount <- decimal
                         skipMany endOfLine -- Maybe not necessary
                         pure $ Instruction (specifyDir text) amount

specifyDir :: Text -> Direction
specifyDir "forward" = Forward
specifyDir "down"    = Down
specifyDir "up"      = Up

------------ TYPES ------------

data Direction = Forward | Down | Up deriving (Show, Eq)
data Instruction = Instruction {
    dir    :: Direction,
    amount :: Int
} deriving Show

type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA xs = uncurry (*) (calculatePosition (0,0) xs)

calculatePosition :: (Int, Int) -> [Instruction] -> (Int, Int)
calculatePosition (h,y) []     = (h,y)
calculatePosition (h,y) [x]    = calculatePosition (decideFromDir (dir x) (h,y) (amount x)) [] 
calculatePosition (h,y) (x:xs) = calculatePosition (decideFromDir (dir x) (h,y) (amount x)) xs 

decideFromDir :: Direction -> (Int, Int) -> Int -> (Int, Int)
decideFromDir Forward (x,y) a = (x + a, y)
decideFromDir Down    (x,y) a = (x, y + a)
decideFromDir Up      (x,y) a = (x, y - a)

------------ PART B ------------
partB :: Input -> OutputB
partB xs = (\(h,y, _) -> h * y) (calculatePosition' (0,0,0) xs)
        
calculatePosition' :: (Int, Int, Int) -> [Instruction] -> (Int, Int, Int)
calculatePosition' (h,y,aim) []     = (h,y,aim)
calculatePosition' (h,y,aim) [x]    = calculatePosition' (decideFromDir' (dir x) (h,y,aim) (amount x)) []
calculatePosition' (h,y,aim) (x:xs) = calculatePosition' (decideFromDir' (dir x) (h,y,aim) (amount x)) xs

decideFromDir' :: Direction -> (Int, Int, Int) -> Int -> (Int, Int, Int)
decideFromDir' Forward (x,y,aim) amount = (x + amount, y + aim * amount, aim)
decideFromDir' Down    (x,y,aim) amount = (x,y,aim + amount)
decideFromDir' Up      (x,y,aim) amount = (x,y,aim - amount)
