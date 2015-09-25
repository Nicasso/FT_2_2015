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

-- random data generator for the datatype Set Int --

randomSetInt :: IO ()
randomSetInt =  do
                  l <- genIntList
                  print (list2set l)

-- QuickCheck to random test (instance)--

instance Arbitrary (Set Int) where 
  arbitrary = do
                numbers <- listOf1 (elements [1..9])
                return (list2set numbers)

-- Properties --

prop_ordered :: (Set Int) -> Bool
prop_ordered s = set2list s == sort (set2list s)  

--prop_NotDuplicates :: (Ord a) => (Set Int) -> Bool
--prop_NotDuplicates s = aux1 (set2list s)

--prop_notDuplicates :: (Ord a) => Set a -> Bool
--prop_notDuplicates s | isEmpty s = True
--                     | elem (head [s]) (tail [s]) = False
--                     | otherwise = prop_notDuplicates (Set (tail [s]))
-- aux functions --

aux1 :: (Ord a) => [a] -> Bool
aux1 [] = True
aux1 [x] = True
aux1 (x:xs) = if elem x xs then False
              else aux1 xs

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

testUnion = verboseCheckWith stdArgs { maxSize = 10 } prop_unionSize

prop_unionSize :: (Set Int) -> (Set Int) -> Bool
prop_unionSize xs ys = length ( set2list (createUnion xs ys) ) == length (nub ((set2list xs) ++ (set2list ys)))

set2list :: Ord a => Set a -> [a]
set2list s | isEmpty s = []
           | otherwise = [(s !!! 0)] ++ set2list (deleteSet (s !!! 0) s)

--testIntersection = verboseCheckWith stdArgs { maxSize = 10 } prop_intersectionSize

--prop_intersectionSize :: (Set Int) -> (Set Int) -> Bool
--prop_intersectionSize xs ys = intersectionElementCheck (createIntersection xs ys) xs ys

intersectionSetCheck :: (Set Int) -> (Set Int) -> (Set Int) -> Bool
intersectionSetCheck (Set (x:xs)) (Set (y:ys)) (Set (z:zs)) = True

intersectionElementCheck :: Int -> (Set Int) -> (Set Int) -> Bool
intersectionElementCheck x ys zs = (inSet x ys || inSet x zs)

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

-- Time spent: 4 hour

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

testTrClos :: Ord a => Rel a -> Bool
testTrClos x = (testIncludes (nub x) (trClos x)) && 
               (testTransitivity (trClos x))

testIncludes :: Ord a => Eq a => Rel a -> Rel a -> Bool
testIncludes [] _ = True
testIncludes (x:xs)(y) = if (elem x y)
                         then testIncludes (xs)(y)
                         else False

testTransitivity :: Ord a => Eq a => Rel a -> Bool
testTransitivity x = isIncludedIn (nub [ (a,d) | (a,b) <- x, (c,d) <- x, b == c, a /= d]) x
 
isIncludedIn :: Ord a => Eq a => Rel a -> Rel a -> Bool
isIncludedIn [] _ = True
isIncludedIn (x:xs)(y) = if (elem x y)
                         then isIncludedIn (xs)(y)
                         else False

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
