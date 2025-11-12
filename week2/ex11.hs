-- Define a function isPrime :: Integer-> Bool that tests primality: n is prime if it has exactly two divisors, namely, 1 and n.
-- Example: isPrime 17 = True, isPrime 21 = False. 
-- Hint: use a list comprehension to get the list of divisors.
isPrime :: Integer -> Bool
isPrime n = length [d | d <- [1..n], n `mod` d == 0] == 2