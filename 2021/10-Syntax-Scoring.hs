
import Data.List(sort)

openclose :: Char -> Char
openclose x
  | x == '('  = ')'
  | x == '['  = ']'
  | x == '{'  = '}'
  | x == '<'  = '>'
  | otherwise = ' '

isopen :: Char -> Bool
isopen o = o `elem` "([{<"

score x
  | x == ')'  = 3
  | x == ']'  = 57
  | x == '}'  = 1197
  | x == '>'  = 25137
  | otherwise = 0

incompleteScore :: Num p => Char -> p
incompleteScore x
  | x == ')'  = 1
  | x == ']'  = 2
  | x == '}'  = 3
  | x == '>'  = 4
  | otherwise = 0

scoreCorrection :: String -> Int
scoreCorrection correction = foldl (\a b -> a*5+b) 0 $ map incompleteScore correction

findMissing :: String -> Int
findMissing line = score $ firstExpected line []
 where
   firstExpected [] _   = ' '
   firstExpected (x:xs) stuff
     | isopen x        = firstExpected xs (openclose x:stuff)
     | x == head stuff = firstExpected xs (tail stuff)
     | otherwise       = x

findCorrection :: String -> String
findCorrection line = missing line []
 where
   missing [] stuff = stuff
   missing (x:xs) stuff
     | isopen x        = missing xs (openclose x:stuff)
     | x == head stuff = missing xs (tail stuff)
     | otherwise       = []

partOne :: [String] -> Int
partOne xs = sum $ map findMissing xs

partTwo :: [String] -> Int
partTwo xs = scores !! ((length scores - 1) `div` 2)
  where
   scores  = sort $ filter (/=0) $ map (scoreCorrection . findCorrection) xs

main = do
  contents <- getContents
  let parenteses = lines contents

  print $ partOne parenteses -- 364389
  print $ partTwo parenteses -- 2870201088
