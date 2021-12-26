
import Data.List.Split (splitOn)
import Data.List (sort)
import Utils (atoiList)

squareFt [x,y,z] = minimum sq + sum (map (* 2) sq)
  where sq = [x*y, y*z, z*x]

rLength xs = product xs + 2 * sum (init $ sort xs)

main = do
  contents <- getContents
  let dimensions = map (atoiList "x") (lines contents)
      wrappingPaper = sum $ map squareFt dimensions
      ribbonLength = sum $ map rLength dimensions

  print wrappingPaper
  print ribbonLength
