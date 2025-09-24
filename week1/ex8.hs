-- O if sempre pede um else!
five :: Int -> String
five n = if n ==5 then "Five" else "Not Five"

five' :: Int -> String
five' n
    | n == 5 = "Five"
    | n == 4 = "Four"
    | otherwise = "Not Five"

-- Pattern match
five'' 5 = "five"
five'' _ = "not five"

-- 'Case' faz pattern match sobre o que está após o case
five''' n = case n of 
    5 -> "five" 
    _ -> "not five"