-- Função de ordem superior: uma função que recebe outra

-- Map tem dois ingredientes: uma função e uma lista. Aplica a função a todos os elementos da lista
-- map(*2)[1,2,3] = [2,4,6]

-- filterodd[1,2,3,4] = [1,3] - função que retorna um booleano - essa função já existe no preludio

-- zipWith(+)[1,2,3][1,2,3] = [2,4,6] - um zip que vai somar os elementos

-- ((+2).(*3))10 = 32 - o ponto significa 'fazer um seguida do outro' - chama-se composição de funções
--   f    y      - a função +2 é aplicada após *3

-- sqrt $ 1 + 2 + 3 - o operador `$` serve para aplicar a função ao resultado da expressão à sua direita.
-- Ou seja, ele substitui parênteses: sqrt $ 1 + 2 + 3 é equivalente a sqrt (1 + 2 + 3).
-- Sem o `$`, você precisaria escrever os parênteses manualmente.
-- `$` ajuda a evitar muitos parênteses e torna o código mais legível.

-- foldl (-) 0 [1,2,3]
-- A função foldl "mastiga" (ou reduz) uma lista, condensando-a em um único valor.
-- Esse valor é do mesmo tipo do acumulador, que guarda o resultado parcial a cada passo.
--
-- Como o fold é "left" (da esquerda), ele começa com o acumulador inicial (0) à esquerda
-- e aplica a função sucessivamente a cada elemento da lista:
--
-- Passos da execução:
-- 1º: ((0 - 1) - 2) - 3
-- 2º: (-1 - 2) - 3
-- 3º: (-3 - 3)
-- Resultado final: -6
--
-- Em forma de lambda, a função passada seria equivalente a:
-- \acc x -> acc - x -- “Função que recebe dois argumentos, acc e x, e devolve acc - x.”
-- O símbolo \ (barra invertida) representa o lambda, ou seja, uma função anônima — sem nome.
-- É como dizer “função que…” em português.
-- acc → é o acumulador, o valor parcial que vai sendo passado de um passo para outro no fold.
-- x → é o elemento atual da lista que está sendo processado.
-- -> → separa os parâmetros da expressão que calcula o resultado.
-- acc - x → é o corpo da função, o cálculo que ela faz.

-- foldr (-) 0 [1,2,3]
--
-- O foldr ("fold right") também reduz uma lista a um único valor, mas o acumulador é aplicado à direita.
--
-- Passos da execução:
-- 1º: 1 - (2 - (3 - 0))
-- 2º: 1 - (2 - 3)
-- 3º: 1 - (-1)
-- Resultado final: 2
--
-- Em forma de lambda, a função passada seria:
-- \x acc -> x - acc -- “Função que recebe dois argumentos, x e acc, e devolve x - acc.”
--
-- Ambos os folds recebem 3 argumentos:
-- 1. A função que combina os elementos da lista
-- 2. O valor inicial do acumulador
-- 3. A lista a ser processada
--
-- Em termos de desempenho:
-- foldl é O(n)
-- foldr pode ser O(n²) dependendo da função e da estrutura de dados.

-- Versão recursiva com acumulador da função reverse
-- rev l = revAuxn l []                    → começa a reversão chamando revAux com a lista original e um acumulador vazio [].
-- revAux [] acc = acc                     → caso base: se a lista acabou, o acumulador já contém o resultado final (a lista invertida).
-- revAux (x:xs) acc = revAux xs (x : acc) → caso recursivo: remove o primeiro elemento x e o adiciona na frente do acumulador, depois continua com o resto xs.

-- Assim, os elementos vão sendo empilhados ao contrário.
-- Essa versão usa uma função auxiliar (revAux) com um acumulador (acc), que vai “construindo” a lista invertida à medida que percorre os elementos.
-- Ex: rev [1,2,3]
--    = revAux [1,2,3] []
--    = revAux [2,3] [1]
--    = revAux [3] [2,1]
--    = revAux [] [3,2,1]
--    = [3,2,1]

-- mais eficiente, mas pode não ser
-- Versão equivalente usando foldl:
-- rev l = foldl(\ acc -> x : acc) [] l
-- passo 1: acc = [], x = 1 → [1]
-- passo 2: acc = [1], x = 2 → [2,1]
-- passo 3: acc = [2,1], x = 3 → [3,2,1]
-- resultado final: [3,2,1]

-- Implementação recursiva simples (naive). Funciona, mas nem sempre é eficiente.
-- rev [] = []
-- rev (x:xs) = rev xs ++ [x]

-- Versão mais genérica, escrita com foldr:
-- rev l = foldr (\x acc -> acc ++ [x]) [] l

-- Currying - é um conceito relacionado a transformar funções de múltiplos argumentos em funções de um único argumento que retornam outras funções.

-- Em Haskell, se uma função f apenas passa o argumento para outra função g, podemos omitir o argumento, pois eles ficam implícitos
-- Point free programming: a função não menciona seus argumentos explicitamente, apenas combina outras funções.
-- f x = g x, assim, f = g, já que tudo que f recebe será igual ao que g recebe
-- Cada passo vai omitindo argumentos explicitamente e usando composição:
-- dec2int = foldl (\acc x -> acc*10 + x) 0
-- foldl (\acc x -> acc*10 + x) 0 [2,3,4]
-- passo 1: acc = 0, x = 2 → 0*10 + 2 = 2
-- passo 2: acc = 2, x = 3 → 2*10 + 3 = 23
-- passo 3: acc = 23, x = 4 → 23*10 + 4 = 234
-- dec2int = foldl (\acc x -> (+) (acc*10) x) 0
-- dec2int = foldl (\acc -> (+) (acc*10)) 0
-- dec2int = foldl (\acc -> (+) ((*10) acc)) 0
-- dec2int = foldl (\acc -> ((+).(*10)) acc) 0
-- dec2int = foldl ((+).(*10)) 0

-- Lazy evaluation significa que as expressões não são avaliadas (executadas) até que o resultado realmente seja necessário. Ou seja, em vez de calcular tudo imediatamente, o programa adianta apenas o suficiente para saber o que precisa no momento.
-- Permite simplificar o código