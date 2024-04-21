import Language.Haskell.TH (prim)
-- Ejercicio 1
-- 1
longitud :: [t] -> Integer
longitud [] = 0
longitud lista = 1 + longitud (tail lista)

-- 2
ultimo :: [t] -> t
ultimo [unicoElemento] = unicoElemento
ultimo lista = ultimo (tail lista)

-- 3
-- Especificación significa sacar último
principio :: [t] -> [t]
principio [] = []
principio [_] = []
principio (elemento1:restoLista) = elemento1 : principio restoLista

-- 4
-- En proceso
reverso :: [t] -> [t]
reverso [] = []
reverso (elemento1:restoLista) = []