-- Problem 9: Special Pythagorean triplet
pythagorean :: [(Integer, Integer, Integer)]
pythagorean = [ (x, y, z) | z <- [1 .. 1000], y <- [1 .. z], x <- [1 .. y], (x^2 + y^2 == z^2) && (x + y + z == 1000)]

