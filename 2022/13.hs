module Thirteen () where


data Packet = List [Packet] | Int Integer deriving (Show)

instance (Read Packet) where
  readsPrec d r = [(List x, rest) | (x, rest) <- readsPrec d r] ++ [(Int x, rest) | (x, rest) <- readsPrec d r]

lst :: Packet
lst = read "[1,[2,[3,[4,[5,6,7]]]],8,9]"

parse = id

partOne lines = "TODO"
partTwo lines = "TODO"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      theLines = map parse input

  print "partOne"
  print $ partOne theLines

  print "partTwo"
  print $ partTwo theLines

  print lst
