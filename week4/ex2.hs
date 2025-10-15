-- I/O é parecido com o Maybe: pode ter qualquer coisa ou não
-- Após inputs e outputs, pode ter ou não valores
-- Monad = Valor + Contexto + Regras de como combinar/compor esses valores
-- ioSucc está no prulude
ioSucc :: IO ()
ioSucc = do
    putStrLn "Write an integer:"
    input <- getLine
    -- transforma integer em string
    let num = read input :: Int
    putStrLn $ "The next number is" ++ show (num + 1) ++ "."

-- irá guardar a string
myGetLine :: IO String
myGetLine = do
    x <- getChar
    if x == '\n'
        then return []
        else do
            -- todos os caracteres lidos, exceto o primeiro
            xs <- myGetLine
            return (x:xs)