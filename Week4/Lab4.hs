module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd

-- 1. --------------------------------------------------


-- 2. --------------------------------------------------

--instance Arbitrary (Set Int) where arbitrary = list2set [1,2]

randomSetInt :: IO ()
randomSetInt =  do                  
                  l <- genIntList              
                  print (genRandomSetInt l emptySet)                            

genRandomSetInt :: [Int] -> Set Int -> Set Int
genRandomSetInt [] s = s
genRandomSetInt (x:xs) s = genRandomSetInt xs (insertSet x s)

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

prop_ordered :: (Ord a) => [a] -> Bool
prop_ordered xs = xs == sort xs

prop_duplicates :: (Ord a) => [a] -> Bool
prop_duplicates [] = True
prop_duplicates [x] = True
prop_duplicates (x:xs) = if elem x xs then False
                             else prop_duplicates xs

-- 3. --------------------------------------------------

-- Time spent: 1 hour

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

-- 4. --------------------------------------------------



-- 5. --------------------------------------------------

-- Time spent: 1 hour

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (x:xs) = sort (nub ([x] ++ [swap x] ++ symClos xs))

-- 6. --------------------------------------------------

-- Time spent: 1 hour

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

complete = [(1,2),(2,3),(3,4)]
p1 = [(1,2)]
p2 = [(2,3)]

trClos :: Ord a => Rel a -> Rel a 
trClos x = do 
            y <- [(nub (x ++ (x @@ x)))]
            if x /= y
            then trClos y 
            else sort x


-- 7. --------------------------------------------------

-- Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R?

-- Yes there is a difference when you change the order of applying the symmetric and transitive closure.
-- Here is the example:

-- Original set: [(1,2),(2,3),(3,4)]
-- Transitive closure: [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
-- Symmetric closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
-- Transitive closure after Symmetric closure: [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
-- Symmetric closure after Transitive closure: [(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)]

-- 8. --------------------------------------------------




-- 9. --------------------------------------------------




-- 10. --------------------------------------------------