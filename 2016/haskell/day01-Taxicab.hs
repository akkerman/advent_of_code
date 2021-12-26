module Taxicab where

import Data.List.Split

type Compass = String
type Direction = Char
type Blocks = Int


compass :: Direction -> Compass -> Compass
compass 'R' xs = tail xs ++ [head xs]
compass 'L' xs = last xs : init xs
compass  _  _  = "NESW"

atoi :: String -> Int
atoi s = read s::Int

parseInput :: [String] -> [[(Direction, Blocks)]]
parseInput = map (map tup .splitOn ", ")
  where
    tup xs = (head xs, atoi (tail xs))

partOne xs = "todo"
partTwo xs = "todo"


main = do
  contents <- getContents
  let input = lines contents
      instructions = head $ parseInput input

  print $ partOne instructions
  -- print $ partTwo instructions
