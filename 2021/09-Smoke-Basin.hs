{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Utils (atoi,pp,flatten)

type Height = Int
type HeightMap = [[Height]]
type Coords = (Int,Int)
type Cell = (Coords,Height)

maxRow = 99
maxCol = 99

getneighbours :: HeightMap -> Coords -> [Height]
getneighbours heightmap (row, col) = map height neighbours
  where
    neighbours = filter withinMap [(row-1, col), (row+1, col), (row,  col-1), (row, col+1)]
    withinMap (r,c) = 0 <= r && 0 <= c && r <= maxRow && c <= maxCol
    height = getHeight heightmap

isLow :: HeightMap -> Coords -> Bool
isLow heightmap (row, col) = current < minimum neighbours
   where
     current = getHeight heightmap (row, col)
     neighbours = getneighbours heightmap (row, col)

getHeight :: HeightMap -> Coords -> Height
getHeight heightmap (r,c) = heightmap !! r !! c

coords :: HeightMap  -> [[Coords]]
coords xs = map (toc [0..maxCol]) [0..maxRow]
  where toc xs n = map (n,) xs

solve1 :: HeightMap -> Height
solve1 input = sum lowpoints + length lowpoints
  where lowpoints = map (getHeight input) $ filter (isLow input) $ concat $ coords input

main = do
  contents <- getContents
  let heightmap = map (map atoi . tail . splitOn "") (lines contents)

  print $ solve1 heightmap -- 15   -- 535
  -- print $  input -- 1134 -- 1122700
