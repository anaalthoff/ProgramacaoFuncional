-- O if sempre pede um else!
five :: Int -> String
five n = if n == 5 then "Five" else "Not Five"

even x = if (x `mod` 2) == 0 
        then "Even"
        else "Odd"

-- Guards
five' :: Int -> String
five' n
  | n == 5 = "Five"
  | n == 4 = "Four"
  | otherwise = "Not Five"

-- Pattern match
five'' 5 = "five"
five'' _ = "not five"

not :: Bool -> Bool
not True = False
not False = True

not' :: Bool -> Bool
not' True = False
not' _ = True

-- 'Case' faz pattern match sobre o que está após o case
five''' n = case n of
  5 -> "five"
  _ -> "not five"