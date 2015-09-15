module Lab2Tests where

import Lab2
import Testing
import System.Random

-- Triangles Testing  ------------------------------------

testTriangle :: (Integer, Integer, Integer, Shape) -> Bool
testTriangle (x, y, z, s) = (triangle x y z == s)

triangleTests :: [Test]
triangleTests = [ Test "triangle test" testTriangle
             [(1, 2, 3, NoTriangle), (1, 1, 1, Equilateral), 
              (1, 2, 2, Isosceles), (8, 6, 10, Rectangular), 
              (3, 5, 7, Other)]]

-- Permutations Testing -------------------------------------------

-- Test with predefined values

testPermutationsManual :: ([Integer], [Integer], Bool) -> Bool
testPermutationsManual (x, y, s) = (isPermutation x y == s)

permutationsTests :: [Test]
permutationsTests = [ Test "permutation test" testPermutationsManual
             [([1,2,3], [2,1,3], True), ([1,3,2,4], [2,4,1,3], True), 
              ([1,3,4], [2,3,4], False), ([1,2], [1,2,2], False), 
              ([8,3,6], [8,3,6], True)]]

-- Automated test process

genUniqueIntList :: IO [Int]
genUniqueIntList = do 
  k <- getRandomInt 10
  n <- getRandomInt 10
  genUniqueIntL k n

genUniqueIntL :: Int -> Int -> IO [Int]
genUniqueIntL _ 0 = return []
genUniqueIntL k n = do 
   x <- getRandomInt k
   y <- randomFlip x
   xs <- genUniqueIntL k (n-1)
   if elem y xs then
    do
   return (xs)
   else
    do
   return (y:xs)

testIsPermutation :: IO ()
testIsPermutation = do
  x <- genUniqueIntList
  if length (perms x) >= 2 then do 
    testTwoPermutations 0 100 (perms x)
  else 
    testIsPermutation

testTwoPermutations :: Eq a => Show a => Integer -> Integer -> [[a]] -> IO ()
testTwoPermutations n m [] = print ("No empty lists allowed")
testTwoPermutations n m (x:xs) = if (testPermutations x (head xs) True == True) then
    do
      print ("Passed on: "++show x++" & "++show (head xs))
      if length xs >= 2 && (n+1) < m then 
        do
          testTwoPermutations (n+1) m xs
        else 
          print ( show (n+1) ++ " test passed")
    else 
      do
      error ("Failed on: "++show x++" & "++show (head xs))

testPermutations :: Eq a => [a] -> [a] -> Bool -> Bool
testPermutations x y z | isPermutation x y == z = True
                       | otherwise = False

-- isDerangment Testing -------------------------------------------------

-- Test with predefined values

testDerangementsManual :: ([Integer], [Integer], Bool) -> Bool
testDerangementsManual (x, y, s) = (isDerangement x y == s)

derangementTests :: [Test]
derangementTests = [ Test "derangement test" testDerangementsManual
             [([1,2,3], [2,1,3], False), ([1,3,2,4], [2,4,1,3], True), 
              ([1,3,4], [2,3,4], False), ([1,2], [1,2,2], False), 
              ([8,3,6], [8,3,6], False)]]

-- Automated test process

testIsDerangement :: IO ()
testIsDerangement = loopTestIsDerangement 0 100

loopTestIsDerangement :: Int -> Int -> IO ()
loopTestIsDerangement x y = do 
  n <- genUniqueIntList
  if length n >= 2 then do 
    testTwoDerangements (buildValidDeran n)
    if (x+1) < y then do 
      loopTestIsDerangement (x+1) y
    else 
      print ( show (x+1) ++ " test passed")
  else 
    loopTestIsDerangement x y

buildValidDeran :: Ord a => Eq a => [a] -> [[a]]
buildValidDeran (x:xs) = [(x:xs)]++[xs++[x]]

testTwoDerangements :: Eq a => Show a => [[a]] -> IO ()
testTwoDerangements [] = print ("No empty lists allowed")
testTwoDerangements (x:xs) = if (testDerangement x (head xs) True == True) then
    do
      print ("Passed on: "++show x++" & "++show (head xs))
    else 
      do
      error ("Failed on: "++show x++" & "++show (head xs))

testDerangement :: Eq a => [a] -> [a] -> Bool -> Bool
testDerangement x y z | isDerangement x y == z = True
                      | otherwise = False

-- Testing IBAN validation -----------------------------------------------

-- Preconditions: 
-- Any string 

-- Postconditions: 
-- True if is a correct IBAN number, False otherwise

testIban :: (String, Bool) -> Bool
testIban (s, b) = iban s == b

ibanTests :: [Test]
ibanTests = [   Test "iban test (all valid)" testIban
               [  ("AL47 2121 1009 0000 0002 3569 8741", True), 
             ("AD12 0001 2030 2003 5910 0100", True),
             ("AT61 1904 3002 3457 3201", True),
             ("AZ21 NABZ 0000 0000 1370 1000 1944", True),
             ("BH67 BMAG 0000 1299 1234 56", True),
             ("BE62 5100 0754 7061", True)
               ], 
               Test "iban test (all invalid)" testIban
               [  ("AL47 2121 1009 0000 0002 3569 8742", False), 
             ("AD12 0001 2030 2003 5910 0101", False),
             ("AT61 1904 3002 3457 3202", False),
             ("AZ21 NABZ 0000 0000 1370 1000 1945", False),
             ("BH67 BMAG 0000 1299 1234 57", False),
             ("BE62 5100 0754 7062", False)
               ]
             ]

-- Auxiliary Functions ----------------------------------------------

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
  b <- getRandomInt 1
  if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

testR :: Int -> Int -> ([Int] -> [Int])
                    -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  if r xs (f xs) then
                    do print ("pass on: " ++ show xs)
                       testR (k+1) n f r
                  else error ("failed test on: " ++ show xs)

testPost :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
testPost f p = testR 1 100 f (\_ -> p)

samelength :: [a] -> [a] -> Bool
samelength xs ys = length xs == length ys