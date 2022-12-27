module One () where

import Utils(atoi)

partOne input = head [ a * b | a <-input, b <- input, a+b == 2020]
partTwo input = head [ a * b * c | a <-input, b <- input, c <- input, a+b+c == 2020]

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      theLines = map atoi input

  print "partOne"
  print $ partOne theLines

  print "partTwo"
  print $ partTwo theLines
