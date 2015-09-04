-- Problem 9: Special Pythagorean triplet

pythagorean = head (filter (/=(0,0,0)) [ pythagoreanChecker x y z | z <- [1 .. 1000], y <- [1 .. z], x <- [1 .. y]])

pythagoreanChecker :: Integer -> Integer -> Integer -> (Integer,Integer,Integer)
pythagoreanChecker x y z | ((x^2) + (y^2) == (z^2)) && (x+y+z) == 1000 = (x,y,z)
                         | otherwise = (0,0,0)