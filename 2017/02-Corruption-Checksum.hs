import Data.List.Split (splitOn)
import Data.List
import Utils (atoiList)

partOne nums = sum $ map (\xs -> maximum xs - minimum xs) nums

partTwo nums = sum $ map (uncurry div . divisiblePair) nums
  where
    pairs l = [(x,y) | x <- l, y <- l, x > y]
    divisiblePair xs = head $ filter isEvenlyDivisible $ pairs xs
    isEvenlyDivisible (x, y) = x `mod` y == 0

main = do
  contents <- getContents
  let nums =  map (atoiList "\t") $ lines contents

  print $ partOne nums
  print $ partTwo nums
