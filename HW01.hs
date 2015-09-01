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

--ANDRE T. VERSION--
-- Get the last digit from a number
--lastDigit :: Integer -> Integer
--lastDigit a = read (getLastChar (show a))

--getLastChar :: String -> String
--getLastChar x = [last x] 

-- Drop the last digit from a number
--dropLastDigit :: Integer -> Integer
--dropLastDigit b = if (b >= 10) then (read (init (show b))) else 0

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits x = if x > 0 
                then reverse ( map (\x -> read [x] :: Integer) (show x) ) 
                else []

--ANDRE T. VERSION--
--toRevDigits :: Integer -> [Integer]
--toRevDigits c = if c > 0 then reverse (digs c) else []

--digs :: Integral x => x -> [x]
--digs 0 = []
--digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleEveryOtherHelper (zip [1..] x)

doubleEveryOtherHelper :: [(Integer, Integer)] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:xs) | even (fst x) = [(snd x)*2] ++ doubleEveryOtherHelper xs
                              | odd (fst x) = [(snd x)] ++ doubleEveryOtherHelper xs
--ANDRE T. VERSION--
--doubleEveryOther :: [Integer] -> [Integer]
--doubleEveryOther [] = []
--doubleEveryOther (x:xs) = [x] ++ [(head xs)*2] ++ doubleEveryOther (tail xs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (map (\x -> read [x] :: Integer) (show x)) + sumDigits xs

--ANDRE T. VERSION--
--sumDigits :: [Integer] -> Integer
--sumDigits [] = 0
--sumDigits (x:xs) = sumD (toDigits x) + sumDigits xs

--sumD :: [Integer] -> Integer
--sumD [] = 0
--sumD b = head b + sumD (tail b)

--toDigits :: Integer -> [Integer]
--toDigits n = reverse (toRevDigits n)

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = rem (sumDigits(doubleEveryOther(toRevDigits x))) 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
