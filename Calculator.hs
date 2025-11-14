-- A basic calculator for arithmetic expressions
-- Based on the example in Chapter 8 of "Programming in Haskell"
-- by Graham Hutton.

-- Pedro Vasconcelos, 2025

module Main where

import Data.Char (isDigit)
import Parsing (Parser, char, many1, parse, satisfy, (<|>))

--
-- a data type for expressions
-- made up from integer numbers, + and *
-- O data type é o "modelo" das expressões — igual uma classe em outras linguagens, mas imutável e puramente estrutural.
--
data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
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
eval (Sub e1 e2) = eval e1 - eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
eval (Mod e1 e2) = eval e1 `mod` eval e2

-- Sobre divisão: o operador / não funciona para Integer em Haskell.
-- Ele é apenas para tipos Fractional (Float, Double), mas o seu parser e o Expr trabalham com Integer.
-- Se tentares usar / com Integer, o compilador vai reclamar: “No instance for Fractional Integer”.
-- O correto para divisão inteira é usar: eval (Div e1 e2) = eval e1 `div` eval e2

-- % não existe em Haskell para Integers.
-- mod é o operador correto para resto de divisão inteira.

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc =
      do char '+'
         t <- term
         exprCont (Add acc t)
    <|> do char '-'
           t <- term
           exprCont (Sub acc t)
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
term = do f <- factor
          termCont f

-- O que faz:
-- term lida com operações de multiplicação, divisão e módulo (ou qualquer operação de “nível médio” de precedência).
-- Ele começa lendo um factor (o número ou expressão entre parênteses) e depois chama termCont para ver se há operadores *, / ou % que continuam o term.

-- Por que isso importa:
-- Em Haskell, a calculadora é recursiva, e queremos respeitar a precedência:
-- 2 + 3 * 4
-- Aqui, 3*4 deve ser calculado antes do 2+....
-- factor cuida de números ou parênteses, enquanto term combina fatores com * / %.

termCont :: Expr -> Parser Expr
termCont acc =
      do char '*'
         f <- factor
         termCont (Mul acc f)
  <|> do char '/'
         t <- factor
         termCont (Div acc t)
  <|> do char '%'
         t <- factor
         termCont (Mod acc t)
  <|> return acc

factor :: Parser Expr
factor =
      do n <- natural
         return (Num n)
  <|> do
        char '('
        e <- expr
        char ')'
        return e
-- O que faz:
-- factor lida com a parte mais básica de uma expressão:
-- Um número natural (123, 5, etc.)
-- Ou uma expressão entre parênteses ((2+3*4))

-- Passo a passo:
-- do n <- natural → tenta ler um número.
-- return (Num n) → cria um nó da árvore AST (Num n).
-- <|> → se não for número, tenta a segunda opção:
-- char '(' → consome '('
-- e <- expr → chama o parser de expressões completo dentro dos parênteses
-- char ')' → consome ')'
-- return e → retorna o resultado da expressão entre parênteses

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