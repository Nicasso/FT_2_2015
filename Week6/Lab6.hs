module Lab6 where

import Data.List
import System.Random
import Lecture6

-- Assignment 1
exM :: Integer -> Integer -> Integer -> Integer
exM x y z = (multiplyList (filterList (makeList x y z 1) y)) `mod` z


makeList :: Integer -> Integer -> Integer -> Integer -> [(Integer,Integer)] 
makeList x y z a | a > y = []
                 | otherwise = makeList x y z (a * 2) ++ [(a,(rem (x^a) z))]

filterList :: [(Integer,Integer)] -> Integer -> [Integer]
filterList [] _ = []
filterList (x:xs) y | y == 0 = []
                    | (fst x) <= y = [(snd x)] ++ filterList xs (y - (fst x))
                    | otherwise = filterList xs y

multiplyList :: [Integer] -> Integer
multiplyList [] = 1
multiplyList (x:xs) = x * multiplyList xs


-- Assignment 2



-- Assignment 3

noPrime :: Integer -> Bool
noPrime n = factors n /= [n]

composites :: [Integer]
composites = filter noPrime [1..]

-- Assignment 4

-- The least composite number that fools the check is 4.
-- And by increasing k the numbers will get larger faster and because of that the calculations take longer.

-- Run using: testComposites 1 composites (or testComposites 2 composites or testComposites 3 composites, whatever your heart desires)

-- Time spent: 1.5 hours

testComposites :: Int -> [Integer] -> IO ()
testComposites k (x:xs) = do 
                            a <- prime_tests_F k x
                            if a then do
                              print (show x ++ " passed")
                              testComposites k xs
                              else do
                                testComposites k xs

-- Assignment 5

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Assignment 6



-- Assignment 7


