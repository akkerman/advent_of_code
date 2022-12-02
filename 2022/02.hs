
module Two () where

import Data.Bifunctor (bimap, first)

data Move = Rock | Paper | Scissors deriving (Show, Eq)

abc :: Char -> Move
abc 'A' = Rock
abc 'B' = Paper
abc 'C' = Scissors
abc _ = error "geen abc"

xyz :: Char -> Move
xyz 'X' = Rock
xyz 'Y' = Paper
xyz 'Z' = Scissors
xyz _ = error "geen xyz"

value :: Move -> Integer
value Rock = 1
value Paper = 2
value Scissors = 3

roundScore :: (Move, Move) -> Integer
roundScore (opponent, self)
  | opponent == self = 3
  | winFrom opponent == self = 6
  | otherwise = 0

score :: (Move, Move) -> Integer
score (opponent, self) = roundScore (opponent, self) + value self

partOne :: [(Char, Char)] -> Integer
partOne = sum . map (score . bimap abc xyz)

loseFrom :: Move -> Move
loseFrom Rock = Scissors
loseFrom Paper = Rock
loseFrom Scissors = Paper

winFrom :: Move -> Move
winFrom Rock = Paper
winFrom Paper = Scissors
winFrom Scissors = Rock

moveFromResult :: (Move, Char) -> (Move, Move)
moveFromResult (m, 'X') = (m, loseFrom m)
moveFromResult (m, 'Y') = (m, m)
moveFromResult (m, 'Z') = (m, winFrom m)
moveFromResult _ = error "invalid result"

partTwo :: [(Char, Char)] -> Integer
partTwo lines = sum $ map (score . moveFromResult . first abc) lines


parse :: String -> (Char, Char)
parse [a,' ',x] = (a,x)
parse _ = error  "wrong input"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      theLines = map parse input

  print "partOne"
  print $ partOne theLines

  print "partTwo"
  print $ partTwo theLines
