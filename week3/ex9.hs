-- Higher order functions
-- Re-write the following definition of a function to compute all positive divisors of an integer using filter instead of a list comprehension. 
-- divisors :: Integer > [Integer] 
-- divisors n = [d | d<-[1..n], n‘mod‘d == 0]

divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d == 0) [1..n]

-- [1..n] → lista de candidatos a divisor
-- \d -> n mod d == 0 → função que retorna True apenas se d divide n
-- filter aplica a função a cada elemento da lista e mantém apenas os que retornam True