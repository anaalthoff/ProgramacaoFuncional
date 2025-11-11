-- A positive integer n is perfect if it equals the sum of it proper divisors. Define a function perfects :: Integer-> [Integer] that computes the list of all perfect number up-to a limit given as argument.
-- Example: perfects 500 = [6,28,496].
-- Hint: use a list comprehension and together with the function defined in the previous exercise.
propDivs :: Integer -> [Integer]
propDivs n = [d | d <- [1 .. n-1], n `mod` d == 0]

perfects :: Integer-> [Integer]
perfects n = [x | x <- [1 .. n], sum(propDivs x) == x]