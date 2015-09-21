module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd

-- 1. --------------------------------------------------


-- 2. --------------------------------------------------

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
symClos (x:xs) = sort (inversePair x ++ symClos xs)

inversePair :: (Eq a) => (a,a) -> Rel a
inversePair (a,b) = if (a /= b) then [(a,b),(b,a)] else [(a,b)]

-- 6. --------------------------------------------------

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

complete = [(1,2),(2,3),(3,4)]
p1 = [(1,2)]
p2 = [(2,3)]

trClos :: Ord a => Rel a -> Rel a 
trClos [] = []
trClos (x:xs) = x:xs


-- 7. --------------------------------------------------




-- 8. --------------------------------------------------




-- 9. --------------------------------------------------




-- 10. --------------------------------------------------