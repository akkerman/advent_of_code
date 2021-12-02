module Dive where

import System.IO (IOMode(ReadMode), openFile, hGetContents)

type Forward = Int
type Depth = Int
type Aim = Int
type Movement = (Forward, Depth, Aim)
type Command = (String, Int)
type Line = String

move ::  Movement -> Command -> Movement
move (f,d,a) ("down", x)    = (f, d, a + x)
move (f,d,a) ("up", x)      = (f, d, a - x)
move (f,d,a) ("forward", x) = (f + x, d + a * x, a)

process :: Movement -> Line -> Movement
process m = move m . asCommand . words

asCommand :: [String] -> Command
asCommand (s:i:_) = (s, read i::Int)

answer :: Movement -> Int
answer (f, d, _) = f * d

main = do  
  let list = []
  handle <- openFile "02-input.txt" ReadMode
  contents <- hGetContents handle

  print $ answer $ foldl process (0,0,0) $ lines contents
