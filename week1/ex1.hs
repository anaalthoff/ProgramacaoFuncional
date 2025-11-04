incr :: Int -> Int
incr x = x + 1

triple :: Int -> Int
triple x = 3 * x

welcome :: String -> String
welcome name = "Hello, " ++ name ++ "! "

count :: String -> String
count str = show (length str) ++ " characters."

-- No terminal, escreve ghci e abre o terminal.
-- ghci :l ex1.hs (carrega o arquivo que deseja)
-- :r (recarrega)
-- :q (sai do ghci)
-- :t (informa o tipo)
-- Assim que carregar, já pode usar os módulos
-- Num e => e -> e é uma forma de aceitar números, float, real, etc. Não apenas inteiro, ou double, ou float, por exemplo

-- Executar no terminal:
-- (a) incr (triple 3)
-- (b) triple (incr (3+1))
-- (c) triple (incr 3 + 1)
-- (d) triple (incr 3) + 1
-- (e) welcome "Harry" ++ welcome "Potter"
-- (f) welcome ("Harry" ++ " Potter")
-- (g) welcome (welcome "Potter")
-- (h) count "Expelliarmus!"
-- (i) count (count "Expelliarmus!")
