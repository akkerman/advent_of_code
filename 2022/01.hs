module One where

import Data.List (sort)
import Data.List.Split (splitWhen)
import Utils (atoi)

parse :: [Char] -> Int
parse "" = 0
parse x = atoi x

sorted :: [Int] -> [Int]
sorted = reverse . sort . map sum . splitWhen (== 0)

partOne :: [Int] -> Int
partOne = head

partTwo :: [Int] -> Int
partTwo = sum . take 3

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      theLines = sorted $ map parse input

  print "partOne"
  print $ partOne theLines

  print "partTwo"
  print $ partTwo theLines
