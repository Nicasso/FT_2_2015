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

-- Our random data generator for Set Int
-- The randomSetInt function is our "from scratch" generator. 
-- First it uses the genIntList function to generate a list of randomly generated integers.
-- After that it will pass that list into the list2set function which will use the list to parse a Set with all the same integers.
-- Finally it will output the Set on the screen using the print function.

-- Time spent: 1.5 hours

-- Run using: randomSetInt

randomSetInt :: IO ()
randomSetInt =  do
                  l <- genIntList
                  print (list2set l)

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

-- The QuickCheck random generator
-- This generator also generates random Sets but this version uses QuickCheck instead of the Monad IO as our previous generator.
-- Here we created an arbitrary instance for the (Set Int) data type.
-- We create a list with random numbers with a length of 1 to 9. 
-- This list is then converted to a Set using the list2set function.

-- Time spent: 5 hours

-- run using: sample (arbitrary :: Gen (Set Int))

instance Arbitrary (Set Int) where 
  arbitrary = do
                numbers <- listOf1 (elements [1..9])
                return (list2set numbers)

-- Properties --

-- Here we have written two properties for testing our Set generator.
-- The first one called prop_ordered converts the Set back to a list and checks if it is ordered.
-- The second one prop_noDuplicates checks if generated set does not contain any duplicates.

-- Time spent: 0.5 hour

prop_ordered :: (Set Int) -> Bool
prop_ordered s = set2list s == sort (set2list s)

-- run using: quickCheckWith stdArgs { maxSize = 20 } prop_ordered

prop_noDuplicates :: (Set Int) -> Bool
prop_noDuplicates s = aux1 (set2list s)

-- run using: quickCheckWith stdArgs { maxSize = 20 } prop_noDuplicates

set2list :: Ord a => Set a -> [a]
set2list s | isEmpty s = []
           | otherwise = [(s !!! 0)] ++ set2list (deleteSet (s !!! 0) s)

-- 3. --------------------------------------------------

-- We created the following set operations: Union, Intersection and Difference.
-- All these functions receive two sets as input and output a single set.
-- The Union function will grab all elements from both sets and create a new set with those elements.
-- The Intersection function will create a new set with only the elements that exists in both sets.
-- The difference function will look for all the elements that are in the first set and not in the second and create a new with with them.

-- Union 

-- Run using: quickCheckWith stdArgs { maxSize = 15 } prop_unionSize

createUnion :: (Ord a) => Set a -> Set a -> Set a 
createUnion (Set [])     set2  =  set2
createUnion (Set (x:xs)) set2  = insertSet x (createUnion (Set xs) set2)

prop_unionSize :: (Set Int) -> (Set Int) -> Bool
prop_unionSize xs ys = length ( set2list (createUnion xs ys) ) == length (nub ((set2list xs) ++ (set2list ys)))

-- Intersection

-- Run using: quickCheckWith stdArgs { maxSize = 15 } prop_intersection

createIntersection :: (Ord a) => Set a -> Set a -> Set a 
createIntersection (Set []) set2 = emptySet
createIntersection (Set (x:xs)) set2 | inSet x set2 = insertSet x (createIntersection (Set xs) set2)
                                     | otherwise = createIntersection (Set xs) set2

prop_intersection :: (Set Int) -> (Set Int) -> Bool
prop_intersection x y = createIntersection x y == createIntersection y x

-- Difference

-- Run using: quickCheckWith stdArgs { maxSize = 15 } prop_difference

-- Checks for each equal value between 2 sets, if it's not inside the difference -- 

createDifference :: (Ord a) => Set a -> Set a -> Set a 
createDifference (Set []) set2 = emptySet
createDifference (Set (x:xs)) set2 | not (inSet x set2) = insertSet x (createDifference (Set xs) set2)
                                   | otherwise = createDifference (Set xs) set2

prop_difference :: (Set Int) -> (Set Int) -> Bool
prop_difference s1 s2 = eachNotInside (takeEqual (set2list s1) (set2list s2)) (set2list (createDifference s1 s2))

takeEqual :: [Int] -> [Int] -> [Int]
takeEqual [] s2 = [] 
takeEqual (x:xs) s2 | elem x s2 = [x] ++ (takeEqual xs s2)
                    | otherwise = (takeEqual xs s2)

eachNotInside :: [Int] -> [Int] -> Bool
eachNotInside [] s2 = True 
eachNotInside (x:xs) s2 | elem x s2 = False
                        | otherwise = True && (eachNotInside xs s2)

-- 4. --------------------------------------------------

-- @TODO: Read and make questions

-- 5. --------------------------------------------------

-- The symClos function calculates the symmetric closure of a set.
-- It does this by recursively creating a new set.
-- It will add each element to the new set, if the element is not reflexive (like (1,1)) we also add the inverse of that element.
-- After that we continue by recursively processing the rest of the list.
-- Because this allows for duplicates to exists we use the function nub to remove all the duplicates.
-- Finally we order the set and then we are done.

-- Run using: symClos [(1,2),(2,3),(3,4)]

-- Time spent: 1 hour

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (x:xs) = sort (nub ([x] ++ (if (fst x /= snd x) then [swap x] else []) ++ symClos xs))

-- 6. --------------------------------------------------

-- The trClos function calculates the transitive closure of a set.
-- The transitive closure is calculated by recursively applying the @@ function on the relations.
-- The @@ function will create all missing transitive relations to the set.
-- So for example when we input a set with [(1,2), (2,3)], @@ will create set with [(1,3)] to make it transitive.
-- By then merging the original set with the set @@ generated we are now one step closer to the transitive closure.
-- We will repeat this process by recursively calling trClos until the set is not modified anymore.
-- This means that @@ did not find any more possible new transitive relations to add, so the set is a transitive closure.
-- At last we sort the set and then we are done.

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

-- Here we have made two random test functions for the symClos function.
-- The prop_symmetricClosure will test the sets for two things.
-- First it will check if the all the relations and the relations in their inverse form are a part of the set.
-- Second it will calculate the expected length of the set and compare it to the actual length of the set.

-- Run using: verboseCheckWith stdArgs { maxSize = 15 } prop_symmetricClosure

-- Time spent: 2.5 hour

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

-- Here we have made a random test function for the trClos function.
-- It will check if all the transitive relations are present in the outcome of trClos.

-- Run using: verboseCheckWith stdArgs { maxSize = 15 } prop_transitiveClosure

-- Time spent: 1 hour

prop_transitiveClosure :: Rel Int -> Bool
prop_transitiveClosure xs = transitiveRelation (xs @@ xs) (trClos xs)

transitiveRelation :: Eq a => Rel a -> Rel a -> Bool
transitiveRelation [] ys = True
transitiveRelation (x:xs) ys | elem x ys = transitiveRelation xs ys
                             | otherwise = False

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