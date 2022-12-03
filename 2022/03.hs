module Three () where

import Utils (splitlist)
import Data.Char(isUpper)

prio :: Char -> Int
prio c
  | isUpper c = fromEnum c - fromEnum 'A' + 27
  | otherwise = fromEnum c - fromEnum 'a' + 1

partOne :: [String] -> Int
partOne = sum . map (prio . common . compartment)
  where
    compartment l = splitAt (length l `div` 2) l
    common (xs, ys) = head [x | x <- xs, x `elem` ys]

partTwo :: [String] -> Int
partTwo = sum . map (prio . common) . splitlist 3
  where
    common [xs, ys, zs] = head [x | x <- xs, x `elem` ys, x `elem` zs]
    common _ = error "incorrect number of compartments"

main :: IO ()
main = do
  contents <- getContents
  let theLines = lines contents

  print "partOne"
  print $ partOne theLines

  print "partTwo"
  print $ partTwo theLines
