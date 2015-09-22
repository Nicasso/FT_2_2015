{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck 
import Test.QuickCheck.Gen
import SetOrd

-- 1. --------------------------------------------------

-- @TODO: Read and make questions

-- 2. --------------------------------------------------

-- @TODO: Create a quickcheck test

instance Arbitrary (Set Int) where arbitrary = do
                                                  numbers <- listOf1 ( elements [1..9] )
                                                  return (list2set numbers)

randomSetInt :: IO ()
randomSetInt =  do
                  l <- genIntList
                  print (list2set l)

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

-- Trying to write test functions. NOT YET DONE!

--testSet :: Form -> Bool
--testSet x = equiv x (convertToCNF x)

--prop_testSet :: IO ()
--prop_testSet = quickCheck (\ x -> testSet x == True)

--prop_ordered :: (Ord Int) => Set Int -> Bool
--prop_ordered xs = xs !!! 0 >= xs !!! 1 && prop_o

--prop_o ::
prop_o x = True 

--prop_notDuplicates :: (Ord Int) => Set Int -> Bool
--prop_notDuplicates s = nub s == s

-- 3. --------------------------------------------------

-- @TODO: Create a test 

-- Time spent: 1 hour

lst1 = [1,2,3] 
lst2 = [1,4,5]

set1 = list2set lst1
set2 = list2set lst2

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

--prop_orderedAfterUnion :: (Ord a) => [Set a] -> Bool
--prop_orderedAfterUnion s = prop_ordered (createUnion (s !! 0) (s !! 1))

--prop_notDuplicatesAfterUnion :: (Ord a) => [Set a] -> Bool
--prop_notDuplicatesAfterUnion s = prop_notDuplicates (createUnion (s !! 0) (s !! 1))

-- 4. --------------------------------------------------

-- @TODO: Read and make questions

-- 5. --------------------------------------------------

-- Time spent: 1 hour

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (x:xs) = sort (nub ([x] ++ (if (fst x /= snd x) then [swap x] else []) ++ symClos xs))

-- 6. --------------------------------------------------

-- Time spent: 1 hour

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos x = do 
            y <- [(nub (x ++ (x @@ x)))]
            if x /= y
            then trClos y 
            else sort x

-- 7. --------------------------------------------------

-- Time spent: 1 hour

-- verboseCheckWith stdArgs { maxSize = 10 } prop_symmetricClosure

testFive :: IO ()
testFive = verboseCheckWith stdArgs { maxSize = 10 } prop_symmetricClosure

prop_symmetricClosure :: Rel Int -> Bool
prop_symmetricClosure xs = testSymClos xs

testSymClos :: Ord a => Rel a -> Bool
testSymClos x = (testSymmetry (nub x) (symClos x)) &&
                (testLength (nub x) (symClos x) 0)

testSymmetry :: Ord a => Eq a => Rel a -> Rel a -> Bool
testSymmetry _ [] = True
testSymmetry (x)(y:ys) = if ((elem y x) || (elem (swap y) x))
                         then testSymmetry (x)(ys)
                         else False
                
testLength :: Ord a => Eq a => Rel a -> Rel a -> Int -> Bool
testLength [] y z = (z == length y)
testLength (x:xs) y z = if (elem (swap x) xs) 
                          then do
                            testLength xs y z
                          else if (fst x == snd x)
                                  then do
                                    testLength xs y (z + 1)
                                  else do
                                    testLength xs y (z + 2)


-- 8. --------------------------------------------------

-- Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R?

-- Yes there is a difference when you change the order of applying the symmetric and transitive closure.
-- Here is the example:

-- Original set: [(1,2),(2,3),(3,4)]
-- Transitive closure: [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
-- Symmetric closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
-- Transitive closure after Symmetric closure: [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
-- Symmetric closure after Transitive closure: [(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)]

-- 9. --------------------------------------------------




-- 10. --------------------------------------------------




-- 11. --------------------------------------------------
