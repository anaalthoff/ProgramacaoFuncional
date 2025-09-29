second :: [a] -> a
second xs = head (tail xs) 

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x) 

pair :: b -> (b, b)
pair x = (x,x) 

double :: Num a => a -> a
double x = 2*x

half :: Fractional a => a -> a
half x = x/2

average :: Fractional a => a -> a -> a
average x y = (x+y)/2 

isLower :: Char -> Bool
isLower x = x >= 'a' && x <= 'z'

-- Ord é uma ordem entre os elementos
inRange :: Ord a => a -> a -> a -> Bool
inRange x lo hi = x >= lo && x <= hi 

-- Eq permite comparar elementos usando '==' e '!='
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs 

-- Precisam ser do mesmo tipo
twice :: (a -> a) -> a -> a
twice f x = f (f x)