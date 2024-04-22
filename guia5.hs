import Language.Haskell.TH (prim)
import Data.Map.Internal.Debug (ordered)
import System.Console.Haskeline (Interrupt)
import System.Win32 (se_GROUP_DEFAULTED)
-- Ejercicio 1
-- 1
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:restoLista) = 1 + longitud restoLista

-- 2
ultimo :: [t] -> t
ultimo [unico] = unico
ultimo (_:restoLista) = ultimo restoLista

-- 3
-- Especificación significa sacar último
principio :: [t] -> [t]
principio [] = []
principio [_] = []
principio (primero:restoLista) = primero : principio restoLista

-- 4
reverso :: [t] -> [t]
reverso [] = []
reverso [unico] = [unico]
reverso (primero:restoLista) = agregarUltimo primero (reverso restoLista)

agregarUltimo :: t -> [t] -> [t]
agregarUltimo elemento [] = [elemento]
agregarUltimo elemento (primero:restoLista) = primero : (agregarUltimo elemento restoLista)

-- Ejercicio 2
-- 1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece elemento [] = False
pertenece elemento (primero:restoLista) = elemento == primero || pertenece elemento restoLista

-- 2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (primero:restoLista) = primero == head restoLista && todosIguales restoLista

-- 3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [_] = True
todosDistintos (primero:restoLista) = noPertence primero restoLista && todosDistintos restoLista

noPertence :: (Eq t) => t -> [t] -> Bool
noPertence elemento lista
    | pertenece elemento lista = False
    | otherwise = True

-- 4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [_] = False
hayRepetidos (primero:restoLista) = pertenece primero restoLista || hayRepetidos restoLista

-- 5
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar elemento (primero:restoLista)
    | elemento == primero = restoLista
    | otherwise = primero : quitar elemento restoLista

-- 6
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos elemento (primero:restoLista)
    | elemento == primero = quitarTodos elemento restoLista
    | otherwise = primero : quitarTodos elemento restoLista

-- 7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (primero:restoLista)
    | pertenece primero restoLista = eliminarRepetidos restoLista
    | otherwise = primero : eliminarRepetidos restoLista

-- 8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos lista1 lista2 = pertenecenTodos lista1 lista2 && pertenecenTodos lista2 lista1

pertenecenTodos :: (Eq t) => [t] -> [t] -> Bool
pertenecenTodos [] lista = True
pertenecenTodos (elemento1:restoElementos) lista = pertenece elemento1 lista && pertenecenTodos restoElementos lista

-- 9
capicua :: (Eq t) => [t] -> Bool
-- capicua lista = lista == reverso lista
capicua [] = True
capicua [_] = True
capicua (primero:restoLista) = primero == ultimo restoLista && capicua (principio restoLista)

-- Ejercicio 3
-- 1

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (primero:restoLista) = primero + sumatoria restoLista

-- 2
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (primero:restoLista) = primero * productoria restoLista

-- 3
maximo :: [Integer] -> Integer
maximo [unico] = unico
maximo (primero:segundo:restoLista)
    | primero > segundo = maximo (primero:restoLista)
    | otherwise = maximo (segundo:restoLista)

-- 4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (primero:restoLista) = (primero+n) : sumarN n restoLista

-- 5
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = []
sumarElPrimero (primero:restoLista) = sumarN primero restoLista

-- 6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo lista = sumarN (ultimo lista) lista

-- 7
pares :: [Integer] -> [Integer]
pares [] = []
pares (primero:restoLista)
    | mod primero 2 == 0 = primero : pares restoLista
    | otherwise = pares restoLista

-- 8
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (primero:restoLista)
    | mod primero n == 0 = primero : multiplosDeN n restoLista
    | otherwise = multiplosDeN n restoLista

-- 9
-- Como usar maximo?
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [unico] = [unico]
ordenar lista = minimo lista : ordenar (quitar (minimo lista) lista)

minimo :: [Integer] -> Integer
minimo [unico] = unico
minimo (primero:segundo:restoLista)
    | primero <= segundo = minimo (primero:restoLista)
    | otherwise = minimo (segundo:restoLista)

