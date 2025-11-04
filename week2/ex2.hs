-- 2.2 The body mass index (BMI) is a simple measure for classifying the weight of adult individuals. The BMI is computed from the individual’s weight and height (in Kg and meters): BMI = weight/height²
-- For example: an individual with 70Kg and 1.70m height has a BMI of 70/1.70² ≈ 24.22. We can classify the result in the following intervals:
-- BMI < 18.5 "underweight"
-- 18.5 ≤ BMI < 25 "normal weight"
-- 25 ≤ BMI < 30 "overweight"
-- 30 ≤ BMI "obese"

-- Write a definition of the functions classifyBMI :: Float-> Float-> String to implement the above classification table. The two function arguments are, respectively, the weight and height.

-- Conditional expression
classifyBMI :: Float -> Float -> String
classifyBMI w h =
  if w / (h * h) < 18.5 then "underweight"
  else if w / (h * h) < 25 then "normal weight"
  else if w / (h * h) < 30 then "overweight"
  else "obese"

-- Guards
classifyBMI'' :: Float -> Float -> String
classifyBMI'' w h
  | w / (h * h) < 18.5 = "underweight"
  | w / (h * h) < 25 = "normal weight"
  | w / (h * h) < 30 = "overweight"
  | otherwise = "obese"