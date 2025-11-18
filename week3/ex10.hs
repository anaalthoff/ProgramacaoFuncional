-- Let us write the isPrimeFast :: Integer-> Bool function using higher-order functions instead of recursion. Recall that n is prime if and only if n is greater than 1 and no number in the range between 2 and ⌊√n⌋ is a divisor of n.
-- Use the higher-order function all to express the “no number in the range...” part of the above condition. To compute the integer part of the square root you can use floor (sqrt (fromIntegral n)).
isPrimeFast :: Integer -> Bool
isPrimeFast n =
    n > 1 && all (\d -> n `mod` d /= 0) [2 .. upper]
  where
    upper = floor (sqrt (fromIntegral n))

-- Explicação linha por linha
-- n > 1 -- garante que o número é maior que 1
-- all (\d -> n `mod` d /= 0) [2 .. upper] 
-- verifica que nenhum número entre 2 e √n divide n
-- upper = floor (sqrt (fromIntegral n))
-- sqrt trabalha com Double → precisamos converter n de Integer para Double com fromIntegral
-- floor → pega a parte inteira da raiz quadrada