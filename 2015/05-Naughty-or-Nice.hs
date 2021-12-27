
import Data.List
import qualified Data.Set as Set


-- | It contains at least three vowels
contains3vowels xs = 3 <= length (filter (`elem` "aeiou") xs)

-- | It contains at least one letter that appears twice in a row
contains2consecutive xs = any ((>1).length) $ group xs

-- | It does not contain the strings ab, cd, pq, or xy
containsForbidden xs = any (`isInfixOf` xs) ["ab", "cd", "pq", "xy"]


-- | It contains a pair of any two letters that appears at least twice in the string without overlapping
containsPairTwice xs = any ((>=2) . length)$ group $ sort $ pairs $ removeOverlap xs
  where removeOverlap xs = concatMap replaceTrioWithPair $ group xs
        replaceTrioWithPair [x,y,z] = [x,y]
        replaceTrioWithPair ys = ys

-- | It contains at least one letter which repeats with exactly one letter between them
containsRepeatLetter xs = any (\[x,y,z] -> x == z) $ trios xs

pairs xs = zipWith (\x y -> [x,y]) xs (tail xs)
trios xs = zipWith (:) xs $ (pairs . tail) xs

partOne input = length $ filter isNice input
  where isNice xs = contains3vowels xs && contains2consecutive xs && not (containsForbidden xs)

partTwo input =  length $ filter isNice input
  where isNice xs = containsRepeatLetter xs && containsPairTwice xs

main = do
  contents <- getContents
  let input = lines contents

  print $ partOne input -- 238
  print $ partTwo input -- not 81, not 54
