module Simulacro where

-- Ejercicio 1
relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (primera:restoRelaciones)
    | ambosComponentesIguales primera = False
    | contieneTupla primera restoRelaciones = False
    | otherwise = True && relacionesValidas restoRelaciones

contieneTupla :: (String,String) -> [(String,String)] -> Bool
contieneTupla _ [] = False
contieneTupla (elemento1,elemento2) ((primero1,primero2):restoTuplas)
    | elemento1 == primero1 && elemento2 == primero2 = True
    | elemento1 == primero2 && elemento2 == primero1 = True
    | otherwise = False || contieneTupla (elemento1,elemento2) restoTuplas


contiene :: (Eq t) => t -> [t] -> Bool
contiene _ [] = False
contiene elemento (primero:restoLista) = elemento == primero || contiene elemento restoLista

ambosComponentesIguales :: (String,String) -> Bool
ambosComponentesIguales (string1,string2) = string1 == string2

-- Ejercicio 2
personas :: [(String, String)] -> [String]
personas relaciones = quitarRepetidos (separarTuplas relaciones)

separarTuplas :: [(String,String)] -> [String]
separarTuplas [] = []
separarTuplas ((string1,string2):restoTuplas) = (string1 : [string2]) ++ separarTuplas restoTuplas

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos (primero:restoLista)
    | contiene primero restoLista = quitarRepetidos restoLista
    | otherwise = primero : quitarRepetidos restoLista

quitar :: String -> [String] -> [String]
quitar _ [] = []
quitar elemento (primero:restoLista)
    | elemento == primero = restoLista
    | otherwise = primero : quitar elemento restoLista

-- Ejercicio 3
amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe persona ((persona1,persona2):restoRelaciones)
    | persona == persona1 = persona2 : amigosDe persona restoRelaciones
    | persona == persona2 = persona1 : amigosDe persona restoRelaciones
    | otherwise = amigosDe persona restoRelaciones

-- Ejercicio 4
personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos relaciones = personaConMasApariciones (contarAparicionesLista (personas relaciones) relaciones)

personaConMasApariciones :: [(String,Integer)] -> String
personaConMasApariciones [(unicaPersona,aparciones)] = unicaPersona
personaConMasApariciones ((persona1,aparciones1):(persona2,aparciones2):restoPersonas)
    | aparciones1 < aparciones2 = personaConMasApariciones ((persona2,aparciones2):restoPersonas)
    | otherwise = personaConMasApariciones ((persona1,aparciones1):restoPersonas)

contarAparicionesLista :: [String] -> [(String,String)] -> [(String,Integer)]
contarAparicionesLista [] _ = []
contarAparicionesLista (persona1:restoPersonas) relaciones = (persona1,contarAparicionesDe persona1 relaciones) : contarAparicionesLista restoPersonas relaciones 

contarAparicionesDe :: String -> [(String,String)] -> Integer
contarAparicionesDe _ [] = 0
contarAparicionesDe persona ((persona1,persona2):restoRelaciones)
    | persona == persona1 || persona == persona2 = 1 + contarAparicionesDe persona restoRelaciones
    | otherwise = contarAparicionesDe persona restoRelaciones