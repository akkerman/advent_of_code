
-- Writing Parsers from Scratch
-- https://www.youtube.com/watch?v=LeoDCiu_GB0 


parse :: [string] -> [(Int,Int,String,String)]
parse xs = map ( splitOn ": "

main = do
  contents <- getContents
  let input = lines contents
      list = parse input
  print list

