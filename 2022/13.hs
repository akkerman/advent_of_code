{-# LANGUAGE InstanceSigs #-}
module Thirteen () where
import Utils (splitlist)
import Data.List


data Packet = List [Packet] | Int Integer deriving Eq

instance Read Packet where
  readsPrec :: Int -> ReadS Packet
  readsPrec d r = [(List x, rest) | (x, rest) <- readsPrec d r] ++ [(Int x, rest) | (x, rest) <- readsPrec d r]

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Int x) (Int y)      = compare x y
  compare (List xs) (List ys)  = compare xs ys
  compare x@(Int _) (List ys)  = compare [x] ys
  compare (List xs) y@(Int _)  = compare xs [y]

lst :: Packet
lst = read "[1,[2,[3,[4,[5,6,7]]]],8,9]"

parse xs "" = xs
parse xs  s = xs ++ [read s :: Packet]

isSmaller :: Integer -> [Packet] -> Integer
isSmaller i [p1,p2] = if p1<p2 then i else 0

partOne :: [Packet] -> Integer
partOne lines = sum $ zipWith isSmaller [1..] $ splitlist 2 lines

divider2 :: Packet
divider2 = read "[[2]]"
divider6 :: Packet
divider6 = read "[[6]]"

partTwo lines = (1+idx2) * (1+idx6)
  where
    sorted = sort $ [divider2, divider6] ++ lines
    Just idx2 = elemIndex divider2 sorted
    Just idx6 = elemIndex divider6 sorted

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      theLines = foldl parse [] input

  print "partOne"
  print $ partOne theLines

  print "partTwo"
  print $ partTwo theLines
