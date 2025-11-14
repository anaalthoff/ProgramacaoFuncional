{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Data.Char (isDigit)
import Parsing (Parser, char, many1, parse, satisfy, (<|>))

--
-- a data type for expressions
-- made up from integer numbers, + and *
-- O data type é o "modelo" das expressões — igual uma classe em outras linguagens, mas imutável e puramente estrutural.
--
data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)

-- A expressão "3 + 4" vira:
-- Add (Num 3) (Num 4)

-- A expressão "2 * 3 * 4" vira:
-- Mul (Mul (Num 2) (Num 3)) (Num 4)

-- a recursive evaluator for expressions
--
eval :: Expr -> Integer
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do
  t <- term
  exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc =
  do
    char '+'
    t <- term
    exprCont (Add acc t)
    <|> return acc

-- Se achar +, continua acumulando.
-- Senão, retorna o valor acumulado (acc).

-- acc significa accumulator (acumulador): guarda a expressão que já foi interpretada até agora, enquanto o parser continua lendo o resto
-- Exemplo:
-- 2 + 3
-- Depois de ler o 2, acc = Num 2.

-- O parser lê o próximo + 3 e faz:
-- acc = Add (Num 2) (Num 3)
-- Depois de ler o 3, acc = Num 5.

term :: Parser Expr
term = do
  f <- factor
  termCont f

termCont :: Expr -> Parser Expr
termCont acc =
  do
    char '*'
    f <- factor
    termCont (Mul acc f)
    <|> return acc

factor :: Parser Expr
factor =
  do
    n <- natural
    return (Num n)
    <|> do
      char '('
      e <- expr
      char ')'
      return e

natural :: Parser Integer
natural = do
  xs <- many1 (satisfy isDigit)
  return (read xs)

----------------------------------------------------------------

main :: IO ()
main =
  do
    txt <- getContents
    calculator (lines txt)

-- | read-eval-print loop
calculator :: [String] -> IO ()
calculator [] = return ()
calculator (l : ls) = do
  putStrLn (evaluate l)
  calculator ls

-- | evaluate a single expression
evaluate :: String -> String
evaluate txt =
  case parse expr txt of
    [(tree, "")] -> show (eval tree)
    _ -> "parse error; try again"