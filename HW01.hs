{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = read ([last (show x)])

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = if x >= 10 
                  then read (init (show x))
                  else 0

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits = undefined

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleEveryOtherHelper (zip [1..4] x)

doubleEveryOtherHelper :: [(Integer, Integer)] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:xs) | even (fst x) = [(snd x)*2] ++ doubleEveryOtherHelper xs
                              | odd (fst x) = [(snd x)] ++ doubleEveryOtherHelper xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = undefined


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = undefined

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined