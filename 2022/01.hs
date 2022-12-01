module One where

import Data.List.Split (splitWhen)
import Data.List (sort)
import Utils (atoi)

parse "" = 0
parse x = atoi x

sorted xs = reverse $ sort $ map sum $ splitWhen (==0) xs

partOne = head
partTwo = sum . (take 3)

main = do
  contents <- getContents
  let input = lines contents
      theLines = sorted $ map parse input

  print "partOne"
  print $ partOne theLines
  print "partTwo"
  print $ partTwo theLines
