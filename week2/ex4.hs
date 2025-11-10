-- 2.4 Write a definition of the exclusive or function xor :: Bool -> Bool -> Bool using multiple equations with patterns.
-- Pattern matching (ou “casamento de padrões”) é um recurso do Haskell (e de outras linguagens funcionais) que permite definir funções de acordo com a forma ou valor dos argumentos.
-- Ou seja, descreve “casos” específicos para os argumentos, e Haskell escolhe automaticamente qual caso usar.

xor :: Bool -> Bool -> Bool
xor False False = False
xor False True  = True
xor True  False = True
xor True  True  = False