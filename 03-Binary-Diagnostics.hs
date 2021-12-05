module BinaryDiagnostics where
-- runhaskell 03-Binary-Diagnostics.hs <03-input.txt
sp :: Num a => [Char] -> [a]
sp [] = []
sp ('1':xs) = [1] ++ sp xs
sp ('0':xs) = [0] ++ sp xs
sp (_:xs) = sp xs

tally :: (Foldable t, Num c) => t [c] -> [c]
tally = foldl (zipWith (+)) $ replicate 12 0

bintodec :: [Bool] -> Int
bintodec xs = bintodec' 0 $ reverse xs
  where 
    bintodec' n []  = 0
    bintodec' n (True:xs) = (2 ^ n) + bintodec' (n+1) xs
    bintodec' n (False:xs) =  bintodec' (n+1) xs

filterLines :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
filterLines pred xss = solve xss 0 
   where 
     solve (xs:[]) n = xs
     solve xss n = solve filtered (n+1)
        where
          numOnes = foldl (+) 0 $ map (\xs -> xs !! n) xss
          numZeroes = length xss - numOnes
          filtered = filter (\xs -> (xs !! n) == (fromEnum (pred numZeroes numOnes))) xss


partOne :: [[Int]] -> Int
partOne xss = (bintodec gamma) * (bintodec epsilon)
  where 
      half = div (length xss) 2
<<<<<<< HEAD
      gamma = map (half <) $ tally xss
=======
      gamma = map (\i -> half < i) $ tally xss
>>>>>>> d242b37 (Day 3: Binary Diagnostics in haskell)
      epsilon = map not gamma

partTwo :: [[Int]] -> Int
partTwo xss = (bintodec oxygen) * (bintodec co2)
  where
    oxygen = map (1==) $ filterLines (<=) xss
    co2 = map (1==) $ filterLines (>) xss

main = do
  contents <- getContents
  let input = lines contents
      theLines = map sp input

  print "partOne"
  print $ partOne theLines
  print "partTwo"
  print $ partTwo theLines
