-- Problem 9: Special Pythagorean triplet

pythagorean = head (filter (/=(0,0,0)) [ pythagoreanChecker x y z | z <- [1 .. 1000], y <- [1 .. z], x <- [1 .. y]])

pythagoreanChecker :: Integer -> Integer -> Integer -> (Integer,Integer,Integer)
pythagoreanChecker x y z | ((x^2) + (y^2) == (z^2)) && (x+y+z) == 1000 = (x,y,z)
                         | otherwise = (0,0,0)

-- Problem 10: 

primes0 :: Integer
primes0 = sum(filter prime0 [2..1999999])

prime0 :: Integer -> Bool
prime0 n | n < 1 = error "not a positive integer"
         | n == 1 = False
         | otherwise = ld n == n

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n = n
        | otherwise = ldf (k+1) n

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0