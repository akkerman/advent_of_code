import Utils

calc mass = mass `div` 3 - 2

calc2 fuel
   | fuel <= 0 = 0
   | otherwise = fuel + calc2 (calc fuel)

main = do
  contents <- getContents
  let input = lines contents
      masses = map atoi input

  print $ sum $ map calc masses
  print $ sum $ map (calc2. calc) masses
