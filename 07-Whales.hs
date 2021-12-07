module Whales where

import Data.List (sortOn)
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
solve calculateFuel input = head $ sortOn snd positionsWithFuel
  where
    positionsWithFuel = map totalFuelOnPosition input
    totalFuelOnPosition pos = (pos, sum $ calculateFuel input pos)

main = do
  contents <- getContents
  let input = atoiList contents

  print $ solve steps input
  print $ solve incfuel input
