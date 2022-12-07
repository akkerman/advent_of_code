module Seven () where

import Utils (atoi)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M


type DirMap = M.Map String Int
type Instruction = [String]
type Path = [String]

parse :: DirMap -> Path -> [Instruction]-> DirMap
parse dirs path (["$", "cd", "/"]:xs)  = parse dirs [] xs
parse dirs path (["$", "cd", ".."]:xs) = parse dirs (init path) xs
parse dirs path (["$", "cd", name]:xs) = parse (M.insertWith (+) (intercalate "/" $ path ++ [name]) 0 dirs) (path ++ [name]) xs
parse dirs path (["$", "ls"]:xs)       = parse dirs path xs
parse dirs path (["dir", name]:xs)     = parse (M.insertWith (+) (intercalate "/" $ path ++ [name]) 0 dirs) path xs
parse dirs path ([size, name]:xs)      = parse (addSizeToParents dirs path (atoi size)) path xs
    where
      addSizeToParents :: DirMap -> Path -> Int -> DirMap
      addSizeToParents dirs [] size = M.insertWith (+) "" size dirs
      addSizeToParents dirs xs size = addSizeToParents (M.insertWith (+) (intercalate "/" xs) size dirs) (init xs) size
parse dirs path (_:xs) = parse dirs path xs
parse dirs _ [] = dirs


partOne :: DirMap -> Int
partOne = sum . M.elems . M.filter (<100000)

partTwo :: DirMap -> Int
partTwo dirs = minimum $ M.elems $ M.filter(>freeup) dirs
    where freeup = 30000000 - unused
          unused = 70000000 - (dirs M.! "")

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
      theLines = map (splitOn " ") input
      dirs = parse M.empty [] theLines

  print "partOne"
  print $ partOne dirs

  print "partTwo"
  print $ partTwo dirs
