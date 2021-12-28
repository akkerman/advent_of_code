
import Data.List.Split
import qualified Data.Set as S
import Utils

data Operation = ON | OFF | TOGGLE deriving (Eq, Show)
type Point = (Int, Int)
type Instruction = (Operation, Point, Point)

parseInput = map (parseLine.splitOn " ")
 where
   parseLine ("turn":xs) = parseLine xs
   parseLine ("on":xs) = parseNums ON xs
   parseLine ("off":xs) = parseNums OFF xs
   parseLine ("toggle":xs) = parseNums TOGGLE xs
   parseNums op [a,_,b] = (op, point a, point b)
   point = tuple . atoiList'
   tuple [a,b] = (a, b)

points (op, (p1, p2), (q1, q2)) = (op, S.fromList [(p,q) | p <- [p1..q1], q <- [p2..q2]])

complement :: Ord a => S.Set a -> S.Set a -> S.Set a
complement a b = S.difference (S.union a b) (S.intersection a b)

partOne instructions = length $ foldl switch S.empty $ map points instructions
  where
    switch acc (ON, points) = S.union acc points
    switch acc (OFF, points) = S.difference acc points
    switch acc (TOGGLE, points) = complement acc points

main = do
  contents <- getContents
  let input = lines contents
      instructions = parseInput input

  print $ partOne instructions
