module Main where

import Data.List (sortOn,nub)
import Utils (atoiList)

type CrabPosition = Int
type FuelCost = Int
type FuelCalculator = [CrabPosition] -> CrabPosition -> [FuelCost]

distance :: CrabPosition -> CrabPosition -> FuelCost
distance a b = abs (a - b)

steps :: FuelCalculator
steps xs pos = map (distance pos) xs

steptofuel :: FuelCost -> FuelCost
steptofuel n = n * (n + 1) `div` 2

incfuel :: FuelCalculator
incfuel xs pos = map steptofuel $ steps xs pos

solve :: FuelCalculator -> [CrabPosition] -> (CrabPosition, FuelCost)
solve calculateFuel input = foldl minTotalFuel (0, totalFuelOnPosition 0) $ nub input
    where totalFuelOnPosition pos = sum $ calculateFuel input pos
          minTotalFuel (p1, c1) pos
           | c < c1    = (pos,c)
           | otherwise = (p1, c1)
           where c = totalFuelOnPosition pos

main = do
  contents <- getContents
  let input = atoiList contents

  print $ solve steps input
  print $ solve incfuel input
