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
testTwoPermutations n m [] = print ("Nope")
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