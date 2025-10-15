-- I/O é parecido com o Maybe: pode ter qualquer coisa ou não
-- Após inputs e outputs, pode ter ou não valores
-- Monard: valor embooleado dentro do contexto
-- ioSucc está no prulude
ioSucc :: IO ()
ioSucc = do
    putStrLn "Write an integer:"
    input <- getLine
    -- transforma integer em string
    let num = read input :: Int
    putStrLn $ "The next number is" ++ show (num + 1) ++ "."