-- A triple (x,y,z) of positive integers is pythagorical if x²+y² = z². Define a function pyths :: Integer-> [(Integer,Integer,Integer)] that computes all pythogorical triples whose components are limited by the argument.
-- Example: pyths 10 = [(3,4,5), (4,3,5), (6,8,10), (8, 6, 10)].
pyths :: Integer-> [(Integer,Integer,Integer)]
pyths p = [(x,y,z) | z <- [1 .. p], y <- [1 .. z], x <- [1 .. y], x*x + y*y == z*z]

-- z <- [1..n] → z é o maior valor (hipotenusa)
-- y <- [1..z] → y ≤ z
-- x <- [1..y] → x ≤ y