{-# LANGUAGE OverloadedStrings #-}

-- install: haskell-puremd5
-- sudo pacman -S haskell-puremd5

import Data.List (isPrefixOf)
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)

md5' :: String -> Int -> String
md5' key num = show $ md5 $ pack $ key ++ show num

partOne key = head $ filter (isPrefixOf "00000" . md5' key) [1..]
partTwo key = head $ filter (isPrefixOf "000000" . md5' key) [1..]

main :: IO()
main = do
  print $ partOne "iwrupvqb" -- 346386
  print $ partTwo "iwrupvqb" -- 9958218
