-- Use the Prelude functions presented (head, tail, length, take, drop and reverse) to answer the following questions.
-- (a) Define a function to get the second element from a list.
-- Example: 1second [1,2,3,4] == 2
-- Investigate what happens if the list has fewer than 2 elements.
first :: [a] -> a
first l = head (tail l)

-- (b) The function last gets the last element of a list.
-- Example: last [1,2,3,4] == 4
-- Show that this function can be defined as a composition of the above functions.
last' :: [a] -> a
last' l = head (reverse l)

last'' :: [a] -> a
last'' l = head (drop (length l - 1) l)

-- (c) Define the init function that removes the last element from a list using the above functions.
-- Example: init [1,2,3,4] == [1,2,3]
init' :: [a] -> [a]
-- take 2 x = [1,2] // Retorna o número de elementos definido, no caso, 2 elementos
init' i = take (length i - 1) i

init'' :: [a] -> [a]
init'' i = reverse (tail (reverse i))

init''' :: [a] -> [a]
init''' i = reverse (drop 1 (reverse i))

-- (d) Define a middle function that gives que the middle element in a list.
-- Example: middle [3,2,1,4,5] == 1
middle :: [a] -> a
middle m = head (drop (length m `div` 2) m)

middle' :: [a] -> a
-- takes an index of any integral type
-- ['a', 'b', 'c'] !! 0    ---    'a'
-- ['a', 'b', 'c'] !! 2    ---    'c'
middle' m = m !! max 0 (length m `div` 2)

-- (e) Define a function checkPalindrome that checks if a string is a palindrome, i.e. if it is equal to its reverse. The result should be a truth value (Bool).
-- Examplos: checkPalindrome "abba" == True checkPalindrome "abra" == False
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome c = c == reverse c