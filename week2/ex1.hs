-- 2.1 Write two definitions, one using conditional expressions and another using guards for the function classify :: Int-> String that gives a qualititive marks for a grade from 0 to 20 according to the following table. 
-- ≤9 "failed" 
-- 10–12 "passed" 
-- 13–15 "good" 
-- 16–18 "very good" 
-- 19–20 "excellent"

classify :: Int -> String
classify n = if n <= 9 then "failed"
            else if n <= 12 then "passed"
            else if n <= 15 then "good"
            else if n <= 18 then "very good"
            else "excellent"

-- Guards
classify' :: Int -> String
classify' n
        | n <= 9 = "failed"
        | n <= 12 = "passed"
        | n <= 15 = "good"
        | n <= 18 = "very good"
        | otherwise = "excellent"

-- Pattern Match (_) não funciona com uma lista, uma gama de valores. Ele compara apenas valor por valor. Por isso não é possível usá-lo se quero verificar se n é menor que 9, pois á vários valores antes do 9.
-- O mesmo ocorre com o 'case'