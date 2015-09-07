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