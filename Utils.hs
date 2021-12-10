module Utils where

import Data.List.Split (splitOn)

atoi :: String -> Int
atoi s = read s::Int

atoiList :: String -> [Int]
atoiList = map atoi . splitOn ","

-- | split a list in sub lists of given length
splitlist  :: Int -> [a] -> [[a]]
splitlist n xs = sp xs
    where
        sp [] = []
        sp xs = take n xs : sp (drop n xs)

-- | flatten a list of list into one list
flatten :: [[a]] -> [a]
flatten xss = [x | xs <- xss, x <-xs]

-- | pretty print a list of lists
pp :: Show a => [[a]] -> IO ()
pp = putStrLn . unlines . map (unwords . map show)
