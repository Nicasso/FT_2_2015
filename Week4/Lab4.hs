
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd

-- 1. Questions of Chapter 4 of The Haskell Road



-- 2. Random data generator for Set Int

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

{-
generateRandomData :: Num a => Ord a => Set a
generateRandomData = do
  x <- genIntList
  list2set x
-}

-- 3 Implement operations for set intersection, set union, and set difference

a = list2set [(1,2),(2,3),(3,3)]
b = list2set [(2,1),(3,1),(1,1)]

c = list2set [1,2,3]
d = list2set [2,3,4]

createUnion :: (Ord a) => Set a -> Set a -> Set a 
createUnion (Set [])     set2  =  set2
createUnion (Set (x:xs)) set2  = insertSet x (createUnion (Set xs) set2)

createIntersection :: (Ord a) => Set a -> Set a -> Set a 
createIntersection (Set []) set2 = emptySet
createIntersection (Set (x:xs)) set2 | inSet x set2 = insertSet x (createIntersection (Set xs) set2)
                                     | otherwise = createIntersection (Set xs) set2

createDifference :: (Ord a) => Set a -> Set a -> Set a 
createDifference (Set []) set2 = emptySet
createDifference (Set (x:xs)) set2 | not (inSet x set2) = insertSet x (createDifference (Set xs) set2)
                                   | otherwise = createDifference (Set xs) set2

-- 4. Questions of Chapter 5 of The Haskell Road



-- 5. Symmetric closure

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos 