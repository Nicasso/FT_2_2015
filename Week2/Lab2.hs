module Lab2 where
 
import Data.List
import System.Random
import Data.Char

-- Recognizing triangles

-- The triangle function can be tested with the testTriangle function in the Lab2Tests.hs file.
-- Run this using: runTests triangleTests

-- We tested the correctness of the triangle function using a list of predefined triangle values and their outcome.
-- By running these values trough the triangle function we expect the given outcome. 
-- If the outcome is equal to the given outcome the test is a success, if not then the triangle function has failed.

-- Time spent: 
-- triangle: 2 hours
-- test: 1 hour

data Shape = NoTriangle | Equilateral 
            | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle  a b c | (a + b <= c) || (b + c <= a) || (c + a <= b) = NoTriangle
                | (a == b) && (b == c) = Equilateral
                | (a == b) || (b == c) || (a == c) = Isosceles
                | (a^2 + b^2 == c^2) || (a^2 + c^2 == b^2) || (b^2 + c^2 == a^2) = Rectangular
                | otherwise = Other

-- ANDRE T. VERSION
triangleA :: Integer -> Integer -> Integer -> Shape
triangleA x y z  | ((x + y <= z) || (x + z <= y) || (z + y <= x)) = NoTriangle
         | (x == y) && (x == z) = Equilateral 
         | (((x == y) && (x /= z)) || ((x == z) && (x /= y)) || ((y == z) && (y /= x))) = Isosceles 
         | (x^2 + y^2 == z^2) || (z^2 + y^2 == x^2) || (x^2 + z^2 == y^2) = Rectangular   
         | otherwise = Other

-- Recognizing Permutations

-- Because isPermutation is itself a test and not a function that manipulates the input in some sort of way like the quicksort example,
-- it is not possible to write proper pre- and post-conditions. 
-- isPermutation can only be tested by giving valid of invalid input in combination with the required result.
-- And this is the method we used in our tests.

-- The isPermutation function can be tested on a predefined list using the permutationsTests function located in the Lab2Tests.hs file.
-- Run test using: RunTests permutationsTests

-- The isPermutation function can also be automatically tested using the testIsPermutation function in the Lab2Tests.hs file
-- Run test using: testIsPermutation
-- It will test a random list of valid permutations on which it will test the isPermutation function.
-- If the isPermutation function also confirms the permutations as correct the method is valid.

-- Time spent: 
-- isPermutation: 1.5 hour
-- Automated test process: 5 hours

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs (y:ys) | length xs /= length (y:ys) = False
                        | length xs == 1 && (head xs /= head (y:ys)) = False
                        | otherwise = isPermutation (delete y xs) ys

-- Recognizing and generating derangements

-- Just like with the isPermutation function it is not possible to write proper pre- and post-conditions.
-- isDerangement can only be tested by giving valid of invalid input in combination with the required result.
-- And this is the method we used in our tests.

-- The isDerangement function can be tested on a predefined list using the derangementTests function located in the Lab2Tests.hs file.
-- Run test using: RunTests derangementTests

-- The isDerangement can be automatically tested using the testIsDerangement function in the Lab2Tests.hs file
-- Run test using: testIsDerangement
-- It will create a random list and alter it so the first element is added to the tail. So [1,2,3] becomes [2,3,1] (a valid derangement)
-- And these derangements are used to test the validity of the isDerangement function.

-- Time spent:
-- isDerangement & deran: 2 hours
-- Automated test process: 4 hours

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs ys = and [ (x `elem` ys && (index x xs /= index x ys)) && (length xs == length ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs

perms :: Ord a => [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

deran :: Int -> [[Int]]
deran n = filter (\ x -> isDerangement x [0..n-1]) (perms [0..n-1])

-- Implementing and testing IBAN validation

-- We could automate the test process for positive tests but that would require a IBAN number generator.
-- For negative tests, we think it's way to difficult to create a function that can be a 100% sure that it randomly generates invalid IBAN numbers.
-- This would make the generation of invalid IBAN numbers nearly impossible.
-- Although we do not have a automated test process we do have written a test which will validate a list of predefined IBAN number.
-- This test is located in the Lab2Test.hs file
-- Run test using: ibanTests

-- Time spent:
-- IBAN: 1.5 hours
-- Test process: 3 hours

iban :: String -> Bool
iban x = (validateCheckDigit(convertIntoNumerics(moveFourCharacters(trim(x)))))

trim :: [Char] -> [Char]
trim [] = []
trim (x:xs) | isLetter x || isDigit x = [x] ++ trim xs
            | otherwise = trim xs

moveFourCharacters :: [Char] -> [Char]
moveFourCharacters x = (drop 4 x) ++ (take 4 x)

convertIntoNumerics :: [Char] -> [Char]
convertIntoNumerics [] = []
convertIntoNumerics (x:xs) | isLetter x = show(ord x - 55) ++ convertIntoNumerics xs
                           | otherwise = [x] ++ convertIntoNumerics xs
                           
validateCheckDigit :: [Char] -> Bool
validateCheckDigit x = ((read x) `mod` 97) == 1