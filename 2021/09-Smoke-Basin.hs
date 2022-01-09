{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Utils (atoi,pp,flatten)

type Height = Int
type HeightMap = [[Height]]
type Coords = (Int,Int)
type Cell = (Coords,Height)

getneighbours :: HeightMap -> Coords -> [Height]
getneighbours heightmap (row, col) = map height neighbours
  where
    (maxCol, maxRow) = maxCoords heightmap
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

maxCoords :: HeightMap -> Coords
maxCoords heightmap = (maxRow, maxCol)
  where maxRow = length heightmap - 1
        maxCol = length (head heightmap) - 1

coords :: Coords -> [Coords]
coords (maxRow, maxCol) = [(row, col) | row <- [0..maxRow], col <-[0..maxCol]]

solve1 :: HeightMap -> Height
solve1 heightmap = sum lowpoints + length lowpoints
  where lowpoints = map (getHeight heightmap) $ filter (isLow heightmap) $ coords $ maxCoords heightmap

main = do
  contents <- getContents
  let heightmap = map (map atoi . tail . splitOn "") (lines contents)

  print $ solve1 heightmap -- 15   -- 535
  -- print $  input -- 1134 -- 1122700
