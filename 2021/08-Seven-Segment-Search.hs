module SevenSegmentSearch where

import Data.List.Split (splitOn)
import Data.List (sort, elemIndex)
import Data.Maybe
import Utils (fromDigits)

type SignalPatterns = [String]
type OutputValues = [String]

parseInput :: [String] -> [(SignalPatterns, OutputValues)]
parseInput = map (tup . splitOn " | ")
  where
    tup [x, y] = (words x, words y)

find :: (a -> Bool) -> [a] -> a
find f = head . filter f

includes :: String -> String -> Bool
includes str = all (`elem` str)

elemIndex' :: [String] -> String -> Int
elemIndex' xs x = fromMaybe (-1) $ elemIndex x xs -- dit gaat stilletje heel erg mis...

-- | put the signal patterns in order
analyze :: SignalPatterns -> SignalPatterns
analyze signals = [zero, one, two, three, four, five, six, seven, eight, nine]
  where
    one   = find ((==2) . length) signals
    four  = find ((==4) . length) signals
    seven = find ((==3) . length) signals
    eight = find ((==7) . length) signals
    three = find (\s -> length s == 5 && includes s one) signals
    six   = find (\s -> length s == 6 && (notElem (head one) s  || notElem (last one) s))  signals
    nine  = find (\s -> length s == 6 && includes s three) signals
    zero  = find (\s -> length s == 6 && s /= six   && s /= nine) signals
    five  = find (\s -> length s == 5 && s /= three && includes nine s) signals
    two   = find (\s -> length s == 5 && s /= three && s /= five) signals

partOne :: [(SignalPatterns, OutputValues)] -> Int
partOne signals = sum $ map matches signals
  where matches (_,output) = length $ filter (\o -> length o `elem` [2,3,4,7]) output

partTwo :: [(SignalPatterns, OutputValues)] -> Int
partTwo signals = sum $ map (fromDigits . decode) signals
 where
   decode (patterns, output) = map (elemIndex' (dict patterns) . sort) output
   dict = map sort . analyze

main = do
  contents <- getContents
  let input = lines contents
      signals = parseInput input

  print $ partOne signals -- 532
  print $ partTwo signals -- 1011284
