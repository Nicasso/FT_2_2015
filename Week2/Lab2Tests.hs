module Lab2Tests where

import Lab2
import Testing

testTriangle :: (Integer, Integer, Integer, Shape) -> Bool
testTriangle (x, y, z, s) = (triangleA x y z == s)

ex1Tests :: [Test]
ex1Tests = [ Test "triangle test" testTriangle
             [(1, 2, 3, NoTriangle), (1, 1, 1, Equilateral), (1, 2, 2, Isosceles), (8, 6, 10, Rectangular), (3, 5, 7, Other)]]