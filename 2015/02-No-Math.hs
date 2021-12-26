
import Data.List.Split (splitOn)
import Utils (atoiList)

squareFt [x,y,z] = minimum sq + sum (map (* 2) sq)
  where sq = [x*y, y*z, z*x]


main = do
  contents <- getContents
  let dimensions = map (atoiList "x") (lines contents)


  print $ sum $ map squareFt dimensions

  -- 865832 is too low
