-- Define a function propDivs :: Integer-> [Integer] using a list compreension that computes the list of proper divisors of a positive integer (d is a proper divisor of n is d divides n and d < n).
-- Example: propDivs 10 = [1, 2, 5].
propDivs :: Integer -> [Integer]
propDivs n = [d | d <- [1 .. n-1], mod n d == 0]

-- [1 .. n-1] é o gerador de d
-- O operador <- serve para gerar elementos a partir de uma lista, por isso usado []
-- "mod n d == 0" pode ser escrito assim: n `mod` d == 0