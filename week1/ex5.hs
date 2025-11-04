-- 1.5 We can compute the area A of a triangle whose sides measure a, b, c using the following formula:
-- A = s(s −a)(s −b)(s −c), where s = (a+b+c)/2.
-- Complete the following Haskell function definition to compute this area.

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c)

triangleArea' :: Float -> Float -> Float -> Float
triangleArea' a b c =
  let s = (a + b + c) / 2
   in sqrt (s * (s - a) * (s - b) * (s - c))