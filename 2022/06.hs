module Six () where

import Data.Set (fromList)

firstSubSet :: Int -> Int -> String -> Int
firstSubSet start size xs
  | uniqChars == size = start + size
  | otherwise = firstSubSet (start + duplicateChars) size (drop duplicateChars xs)
  where
    uniqChars = length $ fromList (take size xs)
    duplicateChars = size - uniqChars

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      stream = head input

  print "partOne"
  print $ firstSubSet 0 4 stream

  print "partTwo"
  print $ firstSubSet 0 14 stream
