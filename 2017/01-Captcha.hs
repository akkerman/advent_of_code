

import Data.List.Split (splitOn)
import Data.List (group, sort)
import Utils (atoi)

partOne nums = sum $ concatMap tail $ filter ((>=2) . length ) $ group (nums ++ [head nums])
partTwo nums = sum $ map fst $ filter (uncurry (==)) $ zip nums (drop half nums ++ take half nums)
  where half = length nums `div` 2

main = do
  contents <- getContents
  -- let nums = head $ map (atoiList "") (lines contents)
  let nums =  map atoi $ filter (/= "") $ splitOn "" $ head $ lines contents

  print $ partOne nums
  print $ partTwo nums
