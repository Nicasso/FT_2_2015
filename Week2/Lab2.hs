{-# OPTIONS_GHC -Wall #-}
module Lab2 where
 
import Data.List
import System.Random

data Shape = NoTriangle | Equilateral 
            | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | (a > (b + c)) || (b > (c + a)) || (c > (a + b)) = NoTriangle
               | (a == b) && (b == c) = Equilateral
               | (a == b) || (b == c) || (a == c) = Isosceles
               | (a^2 + b^2 == c^2) || (a^2 + c^2 == b^2) || (b^2 + c^2 == a^2) = Rectangular
               | otherwise = Other

triangleA :: Integer -> Integer -> Integer -> Shape
triangleA x y z  | ((x + y <= z) || (x + z <= y) || (z + y <= x)) = NoTriangle
				 | (x == y) && (x == z) = Equilateral 
				 | (((x == y) && (x /= z)) || ((x == z) && (x /= y)) || ((y == z) && (y /= x))) = Isosceles	
				 | (x^2 + y^2 == z^2) || (z^2 + y^2 == x^2) || (x^2 + z^2 == y^2) = Rectangular		
				 | otherwise = Other

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = elem a (permutations b)

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement a b = isPermutation a b && notEqualElemsByIndex a b

--deran :: [Integer] -> [[Integer]]
--deran [] = [[]]
--deran (x:xs) = 	concat (map (insrt x) (perms xs)) where
--				insrt x [] = [[x]]
--				insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys) 

notEqualElemsByIndex :: [Integer] -> [Integer] -> Bool
notEqualElemsByIndex [] [] = True
notEqualElemsByIndex (x:xs) (y:ys) = x /= y && notEqualElemsByIndex xs ys

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = 	concat (map (insrt x) (perms xs)) where
				insrt x [] = [[x]]
				insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)	

--iban :: String -> Bool			

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys 