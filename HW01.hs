{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
getLastChar :: String -> String
getLastChar x = [last x] 

lastDigit :: Integer -> Integer
lastDigit x = read ([last (show x)])

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = if x >= 10 
                  then read (init (show x))
                  else 0

--ANDRE T. VERSION--

--lastDigit a = read (getLastChar (show a))

-- Drop the last digit from a number

--dropLastDigit :: Integer -> Integer
--dropLastDigit b = if (b >= 10) then (read (init (show b))) else 0
---------------------

-- Exercise 2 -----------------------------------------
toDigits :: Integer -> [Integer]
toDigits n = reverse (toRevDigits n)

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

toRevDigits :: Integer -> [Integer]
toRevDigits x = while x /< 1 

--ANDRE T. VERSION--
--toRevDigits c = if c > 0 then reverse (digs c) else []
--------------------

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleEveryOtherHelper (zip [1..4] x)

doubleEveryOtherHelper :: [(Integer, Integer)] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:xs) | even (fst x) = [(snd x)*2] ++ doubleEveryOtherHelper xs
                              | odd (fst x) = [(snd x)] ++ doubleEveryOtherHelper xs
--ANDRE T. VERSION--
--doubleEveryOther [] = []
--doubleEveryOther (x:xs) = [x] ++ [(head xs)*2] ++ doubleEveryOther (tail xs)
--------------------

-- Exercise 4 -----------------------------------------
--toInt :: [Integer] -> Integer
--toInt a = read( tail (init (show a) ) )

sumD :: [Integer] -> Integer
sumD [] = 0
sumD b = head b + sumD (tail b)

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumD (toDigits x) + sumDigits xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = mod (sumDigits (doubleEveryOther (toRevDigits n))) 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
