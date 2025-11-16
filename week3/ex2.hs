-- In this exercise we want to implement a prime number test that is more efficient than the one in Worksheet 2:
-- isPrime :: Integer -> Bool
-- isPrime n = length [d | d <- [1..n], n `mod` d == 0] == 2

-- (a) Write a function leastDiv :: Integer-> Integer that computes the smallest divisor greater than 1 of a given number. We need only try candidate divisors d, i.e. numbers d such that n = d × k. However, if d ≥ √n, then k ≤ √n is also a divisor. Hence the smallest divisor will always be less than or equal to √n.
-- Se o divisor chega a sqrt(n) e não achamos nada, então n é primo e o menor divisor é ele mesmo.
leastDiv :: Integer -> Integer
leastDiv n = leastDivFrom n 2 -- 2 é o primeiro divisor possível

leastDivFrom :: Integer -> Integer -> Integer
leastDivFrom n d -- n → o número analisado; d → o divisor atual que testado
    | d * d > n = n          -- se passou da raiz, n é primo
    | n `mod` d == 0 = d         -- achou divisor → retorna 
    | otherwise      = leastDivFrom n (d + 1) -- "Se d NÃO é divisor de n, tente o próximo divisor"
    -- div não testa se é divisor! mod sim.

-- (b) Use leastDiv to define a function isPrimeFast :: Integer-> Bool that checks primality: n is prime if n > 1 and the least divisor of n is n itself. Test that the fast version gives the same results as the original slow one with some examples, e.g. numbers from 1 to 10.
isPrimeFast :: Integer -> Bool
isPrimeFast n = n > 1 && leastDiv n == n
