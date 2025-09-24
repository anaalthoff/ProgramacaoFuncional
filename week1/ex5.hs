triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s*(s-a)*(s-b)*(s-c)) 
    where s = (a+b+c)

triangleArea' a b c = let s = (a+b+c) / 2 in
    sqrt (s*(s-a)*(s-b)*(s-c))

