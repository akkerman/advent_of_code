import Data.List.Split (splitOn)
import Data.List (group,sort)

-- runhaskell 06-Lanternfish.hs <06-input.txt

toi :: String -> Int
toi s = read s::Int

-- | Translate a list of fish states into a list of number of fishes per state
initState :: [Int] -> [Int]
initState fish = map (\xs -> (length xs - 1)) $ group $ sort (fish ++ [0..8])

-- | Calculate one day iteration
nextState :: [Int] -> [Int]
nextState state = shifted ++ [breeders]
  where
    breeders = head state
    rest = tail state
    shifted = (take 6 rest) ++ [( rest !! 6 ) + breeders] ++ (drop 7 rest)


-- | Calculate the next state over a given amount of days
advanceTime :: Int -> [Int] -> [Int]
advanceTime days state = foldl (\state _ -> (nextState state)) state [1..days]

-- | Sum list
countFishes :: [Int] -> Int
countFishes = foldl (+) 0

main = do
  contents <- getContents
  let input = map toi $ head $ map (splitOn ",") $ lines contents
      initialState = initState input

  print $ countFishes $ advanceTime 80 initialState
  print $ countFishes $ advanceTime 265 initialState
