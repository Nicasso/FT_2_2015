module Lab2 where
 
import Data.List
import System.Random

data Shape = NoTriangle | Equilateral 
            | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape

triangle  a b c | not((a + b > c) && (b + c > a) && (c + a > b)) = NoTriangle
                | (a == b) && (b == c) = Equilateral
                | (a == b) || (b == c) || (a == c) = Isosceles
                | (a*a + b*b == c*c) || (a*a + c*c == b*b) || (b*b + c*c == a*a)= Rectangular
                | otherwise = Other

-- ANDRE T. VERSION
triangleA x y z  | ((x + y <= z) || (x + z <= y) || (z + y <= x)) = NoTriangle
				 | (x == y) && (x == z) = Equilateral 
				 | (((x == y) && (x /= z)) || ((x == z) && (x /= y)) || ((y == z) && (y /= x))) = Isosceles	
				 | (x^2 + y^2 == z^2) || (z^2 + y^2 == x^2) || (x^2 + z^2 == y^2) = Rectangular		
				 | otherwise = Other
-------------------
