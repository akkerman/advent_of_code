
import Data.List
import qualified Data.Set as Set

-- | generate a list pairs/coordinates remove duplicates
partOne is = length $ Set.toList $ Set.fromList $ generatePairs [(0,0)] is
  where
    generatePairs pairs [] = pairs
    generatePairs ((x,y):pairs) ('>':is) = generatePairs ((x+1,y):(x,y):pairs) is
    generatePairs ((x,y):pairs) ('<':is) = generatePairs ((x-1,y):(x,y):pairs) is
    generatePairs ((x,y):pairs) ('v':is) = generatePairs ((x,y+1):(x,y):pairs) is
    generatePairs ((x,y):pairs) ('^':is) = generatePairs ((x,y-1):(x,y):pairs) is

-- | same as partOne but alternating two list of coordinates
partTwo is = length $ Set.toList $ Set.fromList $ generatePairs [(0,0)] [(0,0)] is
  where
    generatePairs pairs ys [] = pairs ++ ys
    generatePairs ((x,y):pairs) ys ('>':is) = generatePairs ys ((x+1,y):(x,y):pairs) is
    generatePairs ((x,y):pairs) ys ('<':is) = generatePairs ys ((x-1,y):(x,y):pairs) is
    generatePairs ((x,y):pairs) ys ('v':is) = generatePairs ys ((x,y+1):(x,y):pairs) is
    generatePairs ((x,y):pairs) ys ('^':is) = generatePairs ys ((x,y-1):(x,y):pairs) is


main = do
  contents <- getContents
  let instructions = head $ lines contents

  print $ partOne instructions
  print $ partTwo instructions
