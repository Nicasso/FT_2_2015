module Lab2Tests where

import Lab2
import Testing

testTriangle :: (Integer, Integer, Integer, Shape) -> Bool
testTriangle (x, y, z, s) = (triangle x y z == s)

ex1Tests :: [Test]
ex1Tests = [ Test "triangle test" testTriangle
             [(1, 2, 3, NoTriangle), (1, 1, 1, Equilateral)]]