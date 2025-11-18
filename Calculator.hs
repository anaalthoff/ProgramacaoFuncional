-- A basic calculator for arithmetic expressions
-- Based on the example in Chapter 8 of "Programming in Haskell"
-- by Graham Hutton.

-- Pedro Vasconcelos, 2025

module Main where

-- precisa testar caracteres: isAlpha para variáveis (letras) e isDigit para números
import Data.Char(isAlpha, isDigit)
import Parsing (Parser, char, many1, parse, satisfy, (<|>))

-- novo tipo Name e Env (ambiente) para guardar pares (variável, valor);
type Name = String
type Env = [(Name, Integer)]

-- a data type for expressions
-- made up from integer numbers, + and *
-- O data type é o "modelo" das expressões — igual uma classe em outras linguagens, mas imutável e puramente estrutural.
data Expr = Num Integer
          -- Expr ganhou o construtor Var Name;
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          deriving (Show)

-- foi criado Command para representar 'Assign Name Expr' e 'Eval Expr';
data Command = Assign Name Expr -- → corresponde a name = expr.
          | Eval Expr           -- → corresponde a uma linha só com uma expressão (ex.: 2+3).
          deriving (Show)

-- A expressão "3 + 4" vira:
-- Add (Num 3) (Num 4)

-- A expressão "2 * 3 * 4" vira:
-- Mul (Mul (Num 2) (Num 3)) (Num 4)

-- a recursive evaluator for expressions
-- o avaliador eval agora recebe Env / ambiente (type Env = [(Name, Integer)]) para resolver variáveis;
eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2
eval env (Var x) = case lookup x env of
                      Just v -> v
                      Nothing -> error ("Undefined variable: " ++ x)

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

-- factor tenta três opções (ordem importante):
-- número (natural),
-- variável (variable),
-- expressão entre parênteses.

factor :: Parser Expr
factor =
      do n <- natural
         return (Num n)
  -- verifica se é uma variável   
  <|> do v <- variable
         return (Var v)
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
  -- read converte string para inteiro
  return (read xs)

-- add um parser para variáveis
variable :: Parser Name
variable = many1 (satisfy isAlpha)

-- add parser command (que reconhece x = expr ou só expr);
command = 
      do
        v <- variable
        char '='
        e <- expr
        return (Assign v e)
  <|> do
        e <- expr
        return (Eval e)

-- funções execCommand e execute aplicam comandos ao Env e retornam (output, novoEnv);
-- Caso Eval e: apenas avalia e com eval env e, converte pra String com show e não modifica o env (retorna env inalterado).
execCommand env (Eval e) = (show (eval env e), env)
execCommand env (Assign name e) = 
    let v = eval env e
        env' = (name, v): filter((/= name) . fst) env
       -- coloca (name, v) no começo da lista,
       -- filtra entradas antigas com mesmo name (remoção da antiga associação), garantindo que a nova associação sobrescreva a antiga.
    in (show v, env')
    -- imprime o valor atribuído (ex.: x=1 imprime 1).

----------------------------------------------------------------

main :: IO ()
main =
  do
    txt <- getContents
    -- getContents lê todo o stdin como String
    calculator [] (lines txt)
    -- lines: pega um texto, onde tiver quebra de linha, retorna uma lista de strings
    -- inicia calculator com env = [] (ambiente vazio).

-- | read-eval-print loop
-- calculator é o read-eval-print loop que processa cada linha:
calculator :: Env -> [String] -> IO ()
-- quando lista vazia, termina;
calculator env [] = return ()
calculator env (l : ls) =
  let (out, env') = execute env l 
  in do putStrLn out
        calculator env' ls
-- para cada linha l, chama execute env l que retorna (out, env').
-- imprime out e chamamos recursivamente com env' (propagando alterações feitas por atribuições).

-- | execute a command
execute :: Env -> String -> (String, Env)
execute env txt =
  case parse command txt of
    [(tree, "")] -> execCommand env tree
    _ -> ("parse error; try again", env)