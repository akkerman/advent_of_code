import Data.List.Split
import Data.Matrix

import qualified Data.Set as S
import Utils

data Operation = ON | OFF | TOGGLE deriving (Eq, Show)
type Point = (Int, Int)
type Instruction = (Operation, Point, Point)

parseInput :: [String] -> [Instruction]
parseInput = map (parseLine.splitOn " ")
 where
   parseLine ("turn":"on":xs)   = parseNums ON xs
   parseLine ("turn":"off":xs)  = parseNums OFF xs
   parseLine ("toggle":xs)      = parseNums TOGGLE xs
   parseLine _                  = error "Unknown operation"

   parseNums op [a,"through",b] = (op, point a, point b)
   parseNums _ _                = error "unknown interval"

   point = tuple . atoiList'

   tuple [a,b] = (a, b)
   tuple _     = error "not a tuple"

points :: Instruction -> (Operation, [Point])
points (op, (p1, p2), (q1, q2)) = (op, [(p,q) | p <- [p1..q1], q <- [p2..q2]])

complement :: Ord a => S.Set a -> S.Set a -> S.Set a
complement a b = S.difference (S.union a b) (S.intersection a b)

partOne instructions = length $ foldl switch S.empty $ map points instructions
  where
    switch acc (ON, p) = S.union acc (S.fromList p)
    switch acc (OFF, p) = S.difference acc (S.fromList p)
    switch acc (TOGGLE, p) = complement acc (S.fromList p)


partTwo instructions = foldl switch (zero 1000 1000) $ map points instructions
  where
    switch m (ON, p) = foldl (incElem 1) m p
    switch acc (OFF, p) = foldl decElem acc p
    switch acc (TOGGLE, p) = foldl (incElem 2) acc p
    incElem n m coord = setElem ((m ! coord) + n)  coord m
    decElem m coord = setElem (max ((m ! coord) - 1) 0) coord m


main = do
  contents <- getContents
  let input = lines contents
      instructions = parseInput input

  print $ partOne instructions -- 400410

  print $ partTwo instructions
