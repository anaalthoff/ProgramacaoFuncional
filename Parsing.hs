{-
   A simple purely functional parsing library
   Based on Chapter 8 of "Programming in Haskell" by Graham Hutton
   Pedro Vasconcelos, 2025
-}

-- Parsing é o processo de analisar uma sequência de caracteres (como uma string) para extrair informações estruturadas.
-- É basicamente “entender” ou “traduzir” texto em algo que o programa consegue manipular.
-- Parsing = processar uma string e transformá-la em uma estrutura organizada.

-- Exemplos de parsing:

-- Texto	Estrutura após parser
-- "2 + 3"	Add (Num 2) (Num 3)
-- "4 * 5"	Mul (Num 4) (Num 5)
-- "10"	Num 10 

-- Haskell tem bibliotecas de parsing prontas, como: Text.Parsec, Text.ParserCombinators.ReadP, Text.Megaparsec.
-- Mas o código abaixo é uma implementação didática para aprender como construir parsers do zero, de forma puramente funcional.

-- Parser → o tipo principal de parser.
-- parse → função que executa um parser.
-- <|> → operador de escolha alternativa entre parsers.
-- failure → parser que sempre falha.
-- satisfy → parser que aceita caracteres sob uma condição.
-- getc → parser que lê um único caractere.
-- char → parser que lê um caractere específico.
-- many e many1 → parsers que aplicam repetidamente outro parser.
module Parsing( Parser, parse, (<|>), failure, satisfy, getc, char, many, many1 ) where

-- | the type of parsers:
--  "A parser for things
--   is a function from strings
--   to lists of pairs
--   of things and strings"

-- Um parser é uma função que:
-- Recebe uma string de entrada.
-- Retorna uma lista de pares:
-- O valor parseado (do tipo a).
-- O resto da string que não foi consumido.
-- A lista permite lidar com parsers não determinísticos, que podem ter várias formas válidas de interpretar a entrada.

-- newtype é usado em Haskell quando você quer criar um tipo distinto mas com uma única representação interna.
newtype Parser a
  = P (String -> [(a, String)])

-- Parser é um tipo criado aqui.
-- Ele representa a ideia de parser como uma função: recebe uma string e retorna resultados possíveis.

-- | apply a parser
parse :: Parser a -> String -> [(a, String)]
parse (P f) s = f s
-- Tipo da função:
-- Parser a → um parser que retorna valores do tipo a.
-- String → a entrada de texto que você quer analisar.
-- [(a, String)] → a lista de resultados possíveis, cada um com:
-- o valor parseado (a)
-- o resto da string que não foi consumido.
-- Em resumo: parse aplica um parser a uma string.

-- P f significa: pegue o parser e extraia a função interna f que sabe analisar a string.
-- s é a string de entrada.
-- Depois, chamamos simplesmente: f s
-- Ou seja, aplicamos a função interna f à string s.

-- | Monad instance;
-- this allows us to use do-notation for combining parsers
-- O tipo Parser é uma Monad.
-- Uma Monad é um conceito abstrato que representa computações encadeadas.
-- Isso permite usar funções como: 
-- >>= (bind) → combina parsers sequencialmente.
-- do notation → uma sintaxe limpa para encadear várias operações de parsing.
instance Monad Parser where
  -- return :: a -> Parser a // Cria um parser que não consome nada da string e retorna um valor fixo.
  -- `return' must be defined as `pure' in GHC>=9.6.x
  return = pure 
  
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\s -> case parse p s of
                          [] -> []
                          [(v,s')] -> parse (f v) s')
-- P (\s -> ...): estamos construindo um novo parser (Parser b) usando P. A função interna recebe uma string de entrada s.
-- parse p s: aqui aplicamos o primeiro parser p à entrada s.
-- case parse p s of
-- [] -> []                      -- falha: retorna lista vazia
-- [(v,s')] -> parse (f v) s'    -- sucesso: aplica o próximo parser
-- Caso sucesso ([(v,s')]):
-- v → valor extraído do parser p.
-- s' → resto da string que ainda não foi consumido.
-- f v → cria um novo parser baseado no valor obtido.
-- parse (f v) s' → aplica esse novo parser ao resto da string.

-- | Monad requires instances of Functor and Applicative
-- these are just dummy instances to ensure GHC does not complain
-- Em Haskell, toda Monad precisa ser também um Applicative e um Functor.
-- Ou seja, para declarar instance Monad Parser, você precisa primeiro declarar:
-- instance Functor Parser
-- instance Applicative Parser
-- Aqui o autor está criando instâncias “dummy” só para que o compilador GHC não reclame, porque a Monad já será definida corretamente depois.
instance Functor Parser where
  fmap  = error "Functor instance is incomplete"

instance Applicative Parser where
  pure x = P (\s -> [(x,s)]) 
  (<*>) = error "Applicative instance is incomplete"


-- | alternative between two parsers
-- apply the first parser; if it succeeds return the result;
-- otherwise try the second parser
infixl 5 <|>
-- Declara <|> como um operador infix esquerdamente associativo (infixl) com precedência 5.
-- Isso significa que:
-- p1 <|> p2 <|> p3
-- É interpretado como:
-- (p1 <|> p2) <|> p3
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = P (\s -> case parse p s of
                        [] -> parse q s
                        [(v,s')] -> [(v,s')])

-- p <|> q = P (\s -> ...): estamos criando um novo parser que representa a escolha entre p e q.
-- A função interna recebe uma string s.
-- case parse p s of: aplicamos o primeiro parser p à string s.
-- parse p s retorna:
-- [] -> parse q s         -- se p falhar, aplica q
-- [(v,s')] -> [(v,s')]   -- se p tiver sucesso, retorna o resultado

-- | a parser that always fails
failure :: Parser a
failure = P (\s -> [])
-- \s -> [] → função anônima que:
-- Recebe uma string s (qualquer string de entrada).
-- Retorna [] → lista vazia indica falha no parser.

-- | a parser that return the next character (if any)
getc :: Parser Char
getc = P (\s -> case s of
                  (x:xs) -> [(x, xs)]
                  []     -> [])

-- Parser Char → é um parser que retorna um caractere (Char).
-- Função interna: \s -> case s of ... → função anônima que recebe a string de entrada s e faz uma análise de padrão (case) nela.
-- Análise do case: (x:xs) -> [(x, xs)]
-- Se a string não estiver vazia, ela terá um primeiro caractere x e um resto da string xs.
-- O parser retorna uma lista com um par:
-- x → caractere extraído
-- xs → resto da string
-- [] -> []: Se a string estiver vazia ([]), não há nada para ler.

-- | parse a character that satisfies a predicate (boolean function)
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  x <- getc
  if f x then return x else failure

-- (Char -> Bool) → função que retorna True ou False para um caractere.
-- Parser Char → parser que retorna um caractere (Char).
-- Passo a passo:
-- do
--   x <- getc
-- Aplica o parser getc para ler o próximo caractere da string.
-- x recebe o caractere lido.
-- if f x then return x else failure
-- Aplica a função f ao caractere x.
-- Se f x for True → retorna o caractere com return x.
-- Se f x for False → falha com failure.

-- | parse a given character
-- O objetivo desse parser é ler um caractere específico da string.
char :: Char -> Parser Char
char c = satisfy (==c)

-- Char -> Parser Char: Recebe um caractere c que queremos reconhecer.
-- Retorna um parser que tenta ler exatamente esse caractere da entrada.
-- Como funciona: usa satisfy, que recebe uma função booleana Char -> Bool.
-- (== c) é uma função que retorna True apenas se o caractere for igual a c.
-- Ou seja, char 'a' é equivalente a:
-- satisfy (\x -> x == 'a')
-- Em outras palavras: aplica getc e só aceita o caractere se for igual a c.

-- | apply a parser zero or more times;
-- returns the list of results
-- O parser many aplica outro parser (p) repetidamente.
-- Ele coleta todos os resultados em uma lista.
-- E é não-obrigatório — ou seja, pode dar certo mesmo que o parser não consiga ler nada.
many :: Parser a -> Parser [a]
many p = many1 p <|> return []
-- Parser a -> Parser [a]: recebe um parser de elementos (Parser a).
-- Retorna um parser que produz uma lista de elementos (Parser [a]).
-- Como funciona:
-- Ele tenta aplicar o parser many1 p, que significa “uma ou mais vezes”.
-- Se isso falhar, ele retorna return [], ou seja, uma lista vazia (sem consumir nada).
-- “Tenta consumir o máximo possível. Se não conseguir nada, tudo bem — retorna uma lista vazia.”

-- | apply a parser one or more times;
-- return the list of results
many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

-- many1 consome pelo menos uma vez.
-- Depois tenta consumir mais vezes com many.
-- Concatena os resultados.             