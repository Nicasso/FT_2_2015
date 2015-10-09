module Lab6 where

import Data.List
import System.Random
import Lecture6

import Data.Bits
import Data.Time

-- Assignment 1

-- See the Lecture6.hs file for the implementation of exM

-- Assignment 2

-- The testExmSpeed function will test the improved exM method against the old exM method with random values for x, y, and z.
-- It will calculate the time it will take the functions to respond.
-- As you can see in the test result the performance of the new exM function has improved drastically.

-- Test result:

-- "Calculate: 663846366^12046671 `mod` 15"
-- "Result exM: 6"
-- "Took 0.0010022seconds"
-- "Result exM': 6"
-- "Took 68.1383009seconds"

-- Run using: testExmSpeed

-- Time spent: 1.5 hour

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

-- Simply by checking if the list of factors of a number is not the same as a list containing only the number we can check for composite numbers.

-- Run using: composites

-- Time spent: 0.5 hour

composites :: [Integer]
composites = filter noPrime [1..]

noPrime :: Integer -> Bool
noPrime n = factors n /= [n]

-- Assignment 4

-- The least composite number that fools the check is with k = 1, k = 2, and k = 3 is the number 4.
-- And by increasing k the numbers that will pass the test will get larger faster and because of that the calculations take longer.

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

-- All Carmichael numbers will fool Fermat's primality check.
-- This is because Fermat's little theorem states that if p is a prime then for any integer b, b^p − b in an integer multiple of p.
-- The Carmichael numbers contain this property and for that reason they pass Fermat's test without being actual primes.

-- Run using: carmichael

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Assignment 6

-- We see that the Miller-Rabin primality check also gets fooled by some of the the Carmichael numbers.
-- In comparison to Fermat's primality check, Miller-Rabin primality check at least doesn't let all the Carmichael numbers pass.

-- Run using: testValuesOnMiller 1 carmichael

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

-- Using the Miller-Rabin primality check we where able to find some Mersenne Primes.
-- We begin by calling the discoverMersennePrimesHelper function with a list of primes.
-- And then we check if the form ((2^p)-1) where p is the prime, is also a prime using the primeMR function.
-- If this is true we print the result and go on to the next prime in the list.

-- Some output of the function:
-- "2 is a Mersenne prime"
-- "3 is a Mersenne prime"
-- "5 is a Mersenne prime"
-- "7 is a Mersenne prime"
-- "13 is a Mersenne prime"
-- "17 is a Mersenne prime"
-- "19 is a Mersenne prime"
-- "31 is a Mersenne prime"
-- "61 is a Mersenne prime"
-- "89 is a Mersenne prime"
-- "107 is a Mersenne prime"
-- "127 is a Mersenne prime"
-- "521 is a Mersenne prime"
-- "607 is a Mersenne prime"
-- "1279 is a Mersenne prime"
-- "2203 is a Mersenne prime"
-- "2281 is a Mersenne prime"

-- By comparing our results to the list of Mersenne primes at: http://www.mersenne.org/primes we can
-- see that the Mersenne primes which are discovered by our function are valid Mersenne primes.

-- Run using: discoverMersennePrimes

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