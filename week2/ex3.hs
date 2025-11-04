-- 2.3 Consider two definitions of the max e min from the standard Prelude:
-- max, min :: Ord a => a -> a -> a
-- max x y = if x >= y then x else y
-- min x y = if x <= y then x else y

-- (a) Write similar definitions for two functions max3 e min3 that compute the maximum and minimum between three values.
-- Conditional expression
max3 :: (Ord a) => a -> a -> a -> a
max3 x y z =
  if x >= y && x >= z then x
  else if y >= x && y >= z then y
  else z

-- Guards
max3'' :: (Ord a) => a -> a -> a -> a
max3'' x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | otherwise = z

-- Conditional expression
min3 :: (Ord a) => a -> a -> a -> a
min3 x y z =
  if x <= y && x <= z then x
  else if y <= x && y <= z then y
  else z

-- Guards
min3'' :: (Ord a) => a -> a -> a -> a
min3'' x y z
  | x <= y && x <= z = x
  | y <= x && y <= z = y
  | otherwise = z

-- (b) Observe that the maximum and minimum operations are associative: to compute the maximum of three values we can compute the maximum between two of them and then the maximum between the result and third value. Re-write the function max3 and min3 using this idea and the Prelude max e min functions.

max3''' :: (Ord a) => a -> a -> a -> a
max3''' x y z = max x (max y z)

min3''' :: (Ord a) => a -> a -> a -> a
min3''' x y z = min x (min y z)
