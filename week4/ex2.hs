-- I/O é parecido com o Maybe: pode ter qualquer coisa ou não
-- Após inputs e outputs, pode ter ou não valores
-- Monad = Valor + Contexto + Regras de como combinar/compor esses valores

-- ioSucc está no prulude
-- Tipo da função: é uma ação IO que não retorna nenhum valor útil (()), só faz efeitos colaterais (print na tela e leitura do teclado)
ioSucc :: IO ()
ioSucc = do
  putStrLn "Write an integer:"
  -- escrever no terminal (putStrLn) → IO ()
  input <- getLine
  -- transforma integer em string
  -- getLine :: IO String → lê uma linha do teclado
  -- <- extrai o valor do IO para uso em uma variável dentro do do
  let num = read input :: Int
  -- read converte string em número (String -> Int)
  -- let cria uma variável local pura, sem efeitos de IO
  putStrLn $ "The next number is" ++ show (num + 1) ++ "."
  -- show (num + 1) → transforma Int em String
  -- $ é só sintaxe para evitar parênteses
  -- ++ concatena strings
  -- putStrLn escreve na tela o resultado
  -- Tipo IO ()

-- irá guardar a string
-- IO String - tipo: ação IO que retorna uma string digitada pelo usuário
myGetLine :: IO String
myGetLine = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      -- todos os caracteres lidos, exceto o primeiro
      xs <- myGetLine
      return (x : xs)

-- IO a é um tipo especial que representa uma ação que produz um valor do tipo a no mundo externo, como:
-- ler do teclado (getLine) → IO String
-- escrever no terminal (putStrLn) → IO ()

-- () é o tipo unit, ou seja, "nenhum valor útil", só efeito colateral.

-- Diferente de outras funções puras em Haskell, ações de IO têm efeitos no mundo, então não retornam valores imediatamente.