module GiantSquid where
-- runhaskell 04-Giant-Squid.hs<04-input.txt

import Data.List.Split (splitOn)
import Data.List (isSubsequenceOf, sort, transpose,sortOn)
import Utils (atoi, atoiList, splitlist, flatten)

type Draws = [Int]
type Board = [[Int]]

-- | map a sentence to a list of numbers
toiList :: String -> [Int]
toiList = map atoi . words

-- | parse a list of strings to a list of bingo boards
parseBoards :: [String] -> [Board]
parseBoards = splitlist 5 . map toiList

-- | parse the lines of the file into
-- | - a list of numbers to be called/drawn
-- | - a list of bingo boards
parseInput :: [String] -> (Draws, [Board])
parseInput input = (drawSequence, boards) 
  where
    drawSequence = atoiList $ head input
    boards = parseBoards $ filter (/="") $ tail input

-- | A board is a winning board if 
-- | all numbers in a row or column have been drawn
isWinningBoard :: Draws -> Board -> Bool
isWinningBoard draws board = winningRows || winningColumns
  where 
    sortedDraws = sort draws
    winningRows = any isWin board 
    winningColumns = any isWin $ transpose board
    isWin nums = isSubsequenceOf (sort nums) sortedDraws

-- | Given all draws and a board
-- | calculate the number of draws needed to win
play :: Draws -> Board -> (Board, Int)
play draws board = (board, numDraws)
   where 
     (numDraws, _) = foldl step (0, False) [1..(length draws)]
     step (numDraws, True)  _ = (numDraws, True)
     step (_,        False) n = (n, (isWinningBoard (take n draws) board))

-- | The score of the winning board is
-- | the sum of all unmarked numbers on that board
-- | multiply that sum by the number that was just called when the board won
score :: [Int] -> (Board, Int) -> Int
score draws (board, numDraws) = boardScore * winningDraw
  where 
    boardScore = foldl (+) 0 unmarkedNumbers 
    winningDraw = draws !! (numDraws-1)
    drawsToWin = take numDraws draws
    unmarkedNumbers = filter (\i -> notElem i drawsToWin) $ flatten board

-- | Calculate the number of draws needed to win for all board
-- | sort ascending by number of draws
playAll :: Draws -> [Board] -> [(Board, Int)]
playAll draws boards = sortOn snd $ map (play draws) boards

main = do
  contents <- getContents
  let input = lines contents
      (drawSequence, boards) = parseInput input
      result = playAll drawSequence boards
     
  print $ score drawSequence $ head result
  print $ score drawSequence $ last result
