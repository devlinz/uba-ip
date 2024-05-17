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

-- Ejercicio 4a
-- a
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [unico] = [unico]
sacarBlancosRepetidos (primero:segundo:restoLista)
    | primero == ' ' && segundo == ' ' = sacarBlancosRepetidos (primero:restoLista)
    | otherwise = primero : sacarBlancosRepetidos (segundo:restoLista)

-- b
contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras lista = 1 + (contarPalabrasAux (sacarBlancoInicio (sacarBlancoFin (sacarBlancosRepetidos lista))))

contarPalabrasAux :: [Char] -> Integer
contarPalabrasAux [] = 0
contarPalabrasAux (primero:restoLista)
    | primero == ' ' = 1 + contarPalabrasAux restoLista
    | otherwise = contarPalabrasAux restoLista

sacarBlancoInicio :: [Char] -> [Char]
sacarBlancoInicio [] = []
sacarBlancoInicio (primero:restoLista)
    | primero == ' ' = restoLista
    | otherwise = (primero:restoLista)

sacarBlancoFin :: [Char] -> [Char]
sacarBlancoFin [] = []
sacarBlancoFin [unico]
    | unico == ' ' = []
    | otherwise = [unico]
sacarBlancoFin (primero:restoLista) = (primero:sacarBlancoFin restoLista)

-- c
palabras :: [Char] -> [[Char]]
palabras [] = []
palabras oracion = (primeraPalabra oracion) : palabras (sacarPrimeraPalabra oracion)

primeraPalabra :: [Char] -> [Char]
primeraPalabra [] = []
primeraPalabra oracion = primeraPalabraAux (sacarBlancoInicio (sacarBlancoFin (sacarBlancosRepetidos oracion)))

primeraPalabraAux :: [Char] -> [Char]
primeraPalabraAux [] = []
primeraPalabraAux (letra1:restoOracion)
    | letra1 == ' ' = []
    | otherwise = letra1 : primeraPalabraAux restoOracion

sacarPrimeraPalabra :: [Char] -> [Char]
sacarPrimeraPalabra [] = []
sacarPrimeraPalabra oracion = sacarPrimeraPalabraAux (sacarBlancoInicio (sacarBlancoFin (sacarBlancosRepetidos oracion)))

sacarPrimeraPalabraAux :: [Char] -> [Char]
sacarPrimeraPalabraAux [] = []
sacarPrimeraPalabraAux (letra1:restoOracion)
    | letra1 == ' ' = restoOracion
    | otherwise = sacarPrimeraPalabraAux restoOracion

-- d
palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga oracion
    | sacarPrimeraPalabra oracion == "" = primeraPalabra oracion
    | longitud (primeraPalabra oracion) < longitud (primeraPalabra (sacarPrimeraPalabra oracion)) = palabraMasLarga (sacarPrimeraPalabra oracion)
    | otherwise = palabraMasLarga ((primeraPalabra oracion) ++ [' '] ++ (sacarPrimeraPalabra (sacarPrimeraPalabra oracion)))

-- e
aplanar :: [[Char]] -> [Char]
aplanar [] = []
aplanar (palabra1:restoPalabras) = palabra1 ++ aplanar restoPalabras

-- f
aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [] = []
aplanarConBlancos [unicaPalabra] = unicaPalabra
aplanarConBlancos (palabra1:restoPalabras) = palabra1 ++ " " ++ aplanarConBlancos restoPalabras

-- g
aplanarConNBlancos :: Integer -> [[Char]] -> [Char]
aplanarConNBlancos _ [] = []
aplanarConNBlancos _ [unicaPalabra] = unicaPalabra
aplanarConNBlancos n (palabra1:restoPalabras) = palabra1 ++ (nBlancos n) ++ (aplanarConNBlancos n restoPalabras)

nBlancos :: Integer -> [Char]
nBlancos n
    | n <= 0 = []
    | otherwise = ' ' : nBlancos (n-1)

-- Ejercicio 4b
type Texto = [Char]

palabras2 :: Texto -> [Texto]
palabras2 oracion = palabras oracion

-- etc.

-- Ejercicio 5
-- 1
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada [unicoNumero] = [unicoNumero]
sumaAcumulada (primero:segundo:restoNumeros) = primero : sumaAcumulada ((primero + segundo):restoNumeros)

-- 2
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos [unicoNumero] = [descomponerNEnPrimosDesde unicoNumero 1]
descomponerEnPrimos (primero:restoNumeros) = (descomponerNEnPrimosDesde primero 1) : descomponerEnPrimos restoNumeros

descomponerNEnPrimosDesde :: Integer -> Integer -> [Integer]
descomponerNEnPrimosDesde n i
    | n <= 1 = []
    | mod n (nEsimoPrimo i) == 0 = (nEsimoPrimo i) : descomponerNEnPrimosDesde (div n (nEsimoPrimo i)) 1
    | otherwise = descomponerNEnPrimosDesde n (i+1)

menorDivisor :: Integer ->  Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n i
    | n < i = n
    | mod n i == 0 = i
    | otherwise = menorDivisorDesde n (i+1)

esPrimo :: Integer -> Bool
esPrimo 1 = True
esPrimo n
    | n > 1 = menorDivisor n == n
    | otherwise = esPrimo (0 - n)

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoDesde n 2

nEsimoPrimoDesde :: Integer -> Integer -> Integer
nEsimoPrimoDesde n x
    | esPrimo x && n == 1 = x
    | esPrimo x = nEsimoPrimoDesde (n-1) (x+1)
    | otherwise = nEsimoPrimoDesde n (x+1)