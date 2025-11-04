-- 1.4 The following conditions should hold for three positive values to be sides of a triangle: any of the values must be smaller than the sum of the other two. Complete the following definition of a function that tests these conditions; the result should be a boolean value (True or False).
-- Example: checkTriangle 3 6 2 == False
checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = a < sum [b, c] && b < sum [a, c] && c < sum [b, a]