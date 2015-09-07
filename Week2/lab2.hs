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