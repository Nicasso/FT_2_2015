module Lab2Tests where

import Lab2
import Testing
import System.Random

testTriangle :: (Integer, Integer, Integer, Shape) -> Bool
testTriangle (x, y, z, s) = (triangleA x y z == s)

ex1Tests :: [Test]
ex1Tests = [ Test "triangle test" testTriangle
             [(1, 2, 3, NoTriangle), (1, 1, 1, Equilateral), (1, 2, 2, Isosceles), (8, 6, 10, Rectangular), (3, 5, 7, Other)]]

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
    print ("Success")
    if length xs >= 2 then 
      do
        testTwoPermutations xs
      else 
        print ("DONE")
  else 
    do
    print ("Failure")
    if length xs >= 2 then 
      do
        testTwoPermutations xs
      else 
        print ("WTF2")

testPermutations :: Eq a => [a] -> [a] -> Bool -> Bool
testPermutations x y z | isPermutation x y == z = True--print ("Pass on "+ x)
                       | otherwise = False--print ("Failed test on "+ x)


