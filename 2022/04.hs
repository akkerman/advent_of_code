module Four () where

import Utils (atoiList)

parse :: String -> [Int]
parse = atoiList . map (\c -> if c == '-' then ',' else c)

contains [a1, a2, b1, b2] = if (a1 <= b1 && b2 <= a2) || (b1 <= a1 && a2 <= b2) then 1 else 0

overlaps [a1, a2, b1, b2] = if (a1 <= b1 && a2 >= b1) || (b1 <= a1 && b2 >= a1) then 1 else 0

partOne = sum . map contains
partTwo = sum . map overlaps

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      theLines = map parse input

  print "partOne"
  print $ partOne theLines

  print "partTwo"
  print $ partTwo theLines
