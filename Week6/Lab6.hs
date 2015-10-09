module Lab6 where

import Data.List
import System.Random
import Lecture6

import Data.Bits
import Data.Time

-- Assignment 1

-- See the Lecture6.hs file for the implementation of exM

-- Assignment 2

testExmSpeed :: IO ()
testExmSpeed = do
                 x <- randomRIO (0,1000000000) :: IO Integer
                 y <- randomRIO (10000000,20000000) :: IO Integer
                 z <- randomRIO (2,20) :: IO Integer
                 print ("Calculate: "++show x++"^"++show y++" `mod` "++show z)
                 start1 <- getCurrentTime
                 print ("Result exM: "++show(exM x y z))
                 end1 <- getCurrentTime
                 print ("Took "++show (diffUTCTime end1 start1)++"econds")
                 start2 <- getCurrentTime
                 print ("Result exM': "++show(exM' x y z))
                 end2 <- getCurrentTime
                 print ("Took "++show (diffUTCTime end2 start2)++"econds")

-- Assignment 3

composites :: [Integer]
composites = filter noPrime [1..]

noPrime :: Integer -> Bool
noPrime n = factors n /= [n]

-- Assignment 4

-- The least composite number that fools the check is with k = 1, k = 2, and k = 3 is the number 4.
-- And by increasing k the numbers will get larger faster and because of that the calculations take longer.

-- Run using: testValuesOnFermat 1 composites (or testValuesOnFermat 2 composites or testValuesOnFermat 3 composites, whatever your heart desires)

-- Time spent: 1.5 hours

testValuesOnFermat :: Int -> [Integer] -> IO ()
testValuesOnFermat k [] = print ("Done")
testValuesOnFermat k (x:xs) = do 
                            a <- prime_tests_F k x
                            if a then do
                              print (show x ++ " passed")
                              testValuesOnFermat k xs
                              else do
                                testValuesOnFermat k xs

-- Assignment 5

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Assignment 6

testValuesOnMiller :: Int -> [Integer] -> IO ()
testValuesOnMiller k [] = print ("Done")
testValuesOnMiller k (x:xs) = do 
                            a <- primeMR k x
                            if a then do
                              print (show x ++ " passed")
                              testValuesOnMiller k xs
                              else do
                                testValuesOnMiller k xs

-- Assignment 7

discoverMersennePrimes :: IO ()
discoverMersennePrimes = discoverMersennePrimesHelper primes

discoverMersennePrimesHelper :: [Integer] -> IO ()
discoverMersennePrimesHelper (x:xs) = do
                                  a <- discoverMersennePrimesChecker x
                                  if a then
                                    do
                                      print (show x ++" is a Mersenne prime")
                                      discoverMersennePrimesHelper xs
                                    else
                                      do
                                        discoverMersennePrimesHelper xs

discoverMersennePrimesChecker :: Integer -> IO Bool
discoverMersennePrimesChecker x = do
                                   b <- primeMR 1 ((2^x)-1)
                                   if b == True then 
                                     return True
                                     else
                                       return False