module Sweep where

import System.IO (IOMode(ReadMode), openFile, hGetContents)

answer xs = length $ filter id $ zipWith (<) xs (tail xs)
window xs = map (\(x,y,z) -> x+y+z) $ zip3 xs (tail xs) (drop 2 xs)

asInt i = read i::Int

main = do
  handle <- openFile "01-input.txt" ReadMode
  contents <- hGetContents handle
  print $answer $ window $ map asInt $ lines contents
