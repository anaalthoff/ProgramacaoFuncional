-- The median of three values is the middle value when we place them in ascending order. For example: median 2 3 (-1) == 2.
-- (a) Write a definition of the median function that determines the median of any three values. What is its most general type? Note that we only need comparisons to determine the median.

-- Tipo mais geral:
median :: Ord a => a -> a -> a -> a
median x y z =
    if (x <= y && y <= z) || (z <= y && y <= x) then y
    else if (y <= x && x <= z) || (z <= x && x <= y) then x
    else z

-- guards
median' :: (Ord a) => a -> a -> a -> a
median' x y z
  | (x <= y && y <= z) || (z <= y && y <= x) = y
  | (y <= x && x <= z) || (z <= x && x <= y) = x
  | otherwise = z

-- (b) Instead of defining the median using comparisons you could use the following idea: add all three values e subtract the largest and smallest values. Re-define median this way. What the most general type for this new definition?
median'' :: (Ord a, Num a) => a -> a -> a -> a
median'' x y z = sum[x,y,z] - max x (max y z) - min x (min y z)

-- sum espera uma lista, usar []
