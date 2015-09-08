module Lab2Tests where

import Lab2
import Testing
import System.Random

-- Testing recognize triangles

testTriangle :: (Integer, Integer, Integer, Shape) -> Bool
testTriangle (x, y, z, s) = (triangleA x y z == s)

triangleTests :: [Test]
triangleTests = [ Test "triangle test" testTriangle
             [(1, 2, 3, NoTriangle), (1, 1, 1, Equilateral), (1, 2, 2, Isosceles), (8, 6, 10, Rectangular), (3, 5, 7, Other)]]
-- 

-- Permutations Testing

-- Preconditions: 
-- List have the same length

-- Postconditions: 
-- List has the same length
-- Elements don't stand on the same

genIntList2 :: IO [Int]
genIntList2 = do 
  k <- getRandomInt 10
  n <- getRandomInt 10
  getIntL2 k n

getIntL2 :: Int -> Int -> IO [Int]
getIntL2 _ 0 = return []
getIntL2 k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL2 k (n-1)
   if elem y xs then
    do
   return (xs)
   else
    do
   return (y:xs)

startPerms :: [Integer]
startPerms = undefined

initTestPermutations :: [[Integer]]
initTestPermutations = perms [1..100]



testTwoPermutations :: [[Integer]] -> IO ()
testTwoPermutations [] = print ("ÉMPTY")
testTwoPermutations (x:xs) = if (testPermutations x (head xs) True == True) then
  do
    print ("Passed on: "++show x++" & "++show (head xs))
    if length xs >= 2 then 
      do
        testTwoPermutations xs
      else 
        print ("Done")
  else 
    do
    print ("Failed on: "++show x++" & "++show (head xs))
    if length xs >= 2 then 
      do
        testTwoPermutations xs
      else 
        print ("WTF2")

testPermutations :: Eq a => [a] -> [a] -> Bool -> Bool
testPermutations x y z | isPermutation x y == z = True--print ("Pass on "+ x)
                       | otherwise = False--print ("Failed test on "+ x)

-- Testing IBAN validation -----------------------------------------------

testIban :: (String, Bool) -> Bool
testIban (s, b) = iban s == b

ibanTests :: [Test]
ibanTests = [	 Test "iban test (all valid)" testIban
	             [	("AL47 2121 1009 0000 0002 3569 8741", True), 
	     			("AD12 0001 2030 2003 5910 0100", True),
	     			("AT61 1904 3002 3457 3201", True),
	     			("AZ21 NABZ 0000 0000 1370 1000 1944", True),
	     			("BH67 BMAG 0000 1299 1234 56", True),
	     			("BE62 5100 0754 7061", True)
	             ], 
	             Test "iban test (all invalid)" testIban
	             [	("AL47 2121 1009 0000 0002 3569 8742", False), 
	     			("AD12 0001 2030 2003 5910 0101", False),
	     			("AT61 1904 3002 3457 3202", False),
	     			("AZ21 NABZ 0000 0000 1370 1000 1945", False),
	     			("BH67 BMAG 0000 1299 1234 57", False),
	     			("BE62 5100 0754 7062", False)
	             ]
             ]

-- Can we automate the test process?
--
-- Yes, for positive tests it's possible if we use a iban number generator.
-- For negative tests, we think it's way to difficult to create a function that we have 100% sure that randomly generates invalid iban numbers

-- AUXILIARY FUNCTIONS ----------------------------------------------

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

---------------------------------------------------------------------