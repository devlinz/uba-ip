module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {Grupo Haskell}
-- Integrante1: { 46347796, Lopez Leandro}
-- Integrante2: { 46105715, Miramontes Lucia}
-- Integrante3: { 46287537, Lamblot Camila}
-- Integrante4: { 46701161, Matascuso Marcos}
-- Integrantes que abandonaron la materia: {}

-- I. Cifrado Cesar
-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula c = ord c >= ord 'a' && ord c <= ord 'z'

-- EJ 2
letraANatural :: Char -> Int
letraANatural c = ord c - ord 'a'

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar c n
    | esMinuscula c = numeroAMinuscula (ord c + n)
    | otherwise = c

numeroAMinuscula :: Int -> Char
numeroAMinuscula n
    | n > ord 'z' = numeroAMinuscula (n-26)
    | n < ord 'a' = numeroAMinuscula (n+26)
    | otherwise = chr n

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (letra1:restoLetras) n = (desplazar letra1 n) : (cifrar restoLetras n)

-- EJ 5
descifrar :: String -> Int -> String
descifrar texto n = cifrar texto (-n)

-- EJ 6
cifrarLista :: [String] -> [String]
--cifrarLista _ = ["compu", "mbcp", "kpvtq"]
cifrarLista textos = cifrarListaDesdeN textos 0

cifrarListaDesdeN :: [String] -> Int -> [String]
cifrarListaDesdeN [] _ = []
cifrarListaDesdeN (texto1:restoTextos) n = cifrar texto1 n : cifrarListaDesdeN restoTextos (n+1) 

-- EJ 7
frecuencia :: String -> [Float]
frecuencia texto = frecuenciaDeMinusculasDesde 0 texto

frecuenciaDeMinusculasDesde :: Int -> String -> [Float]
frecuenciaDeMinusculasDesde 26 _ = []
frecuenciaDeMinusculasDesde n texto = frecuenciaDeChar (chr (n+ord 'a')) texto : frecuenciaDeMinusculasDesde (n+1) texto

frecuenciaDeChar :: Char -> String -> Float
frecuenciaDeChar c texto = (fromIntegral (apariciones c texto) / fromIntegral (longitud texto)) * 100

apariciones :: Char -> String -> Int
apariciones _ [] = 0
apariciones c (letra1:restoLetras)
    | c == letra1 = 1 + apariciones c restoLetras
    | otherwise = apariciones c restoLetras

longitud :: String -> Int
longitud [] = 0
longitud (letra1:restoLetras) = 1 + longitud restoLetras

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente palabra n = elegirMayorFrecuencia (agruparConLetras (frecuencia (cifrar palabra n))) 

agruparConLetras :: [Float] -> [(Char,Float)]
agruparConLetras frecuencias = agruparConLetrasDesde frecuencias 'a'

agruparConLetrasDesde :: [Float] -> Char -> [(Char,Float)]
agruparConLetrasDesde [] _ = []
agruparConLetrasDesde (frec1:resto) letra = (letra, frec1) : agruparConLetrasDesde resto (chr (ord letra + 1))

elegirMayorFrecuencia :: [(Char,Float)] -> (Char,Float)
elegirMayorFrecuencia [unicaTupla] = unicaTupla
elegirMayorFrecuencia ((letra,frecuencia):(letra2,frecuencia2):resto)
    | frecuencia >= frecuencia2 = elegirMayorFrecuencia ((letra,frecuencia):resto)
    | otherwise = elegirMayorFrecuencia ((letra2,frecuencia2):resto)

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado palabra1 palabra2 = esDescifradoDesde palabra1 palabra2 26

esDescifradoDesde :: String -> String -> Int -> Bool
esDescifradoDesde _ _ 0 = False
esDescifradoDesde palabra1 palabra2 n
    | palabra2 == cifrar palabra1 n = True
    | otherwise = esDescifradoDesde palabra1 palabra2 (n-1)

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
--todosLosDescifrados (palabra1:resto) = todosLosParesDescifrados (palabra1:resto) ++ todosLosDescifrados resto
todosLosDescifrados palabras = todosLosDescifradosAux palabras palabras

todosLosDescifradosAux :: [String] -> [String] -> [(String,String)]
todosLosDescifradosAux [_] _ = []
todosLosDescifradosAux (palabra1:resto) comparar = todosLosParesDescifrados (palabra1:comparar) ++ todosLosDescifradosAux resto comparar 

todosLosParesDescifrados :: [String] -> [(String,String)]
todosLosParesDescifrados [] = []
todosLosParesDescifrados [_] = []
todosLosParesDescifrados (palabra1:palabra2:resto)
    | palabra1 == palabra2 = todosLosParesDescifrados (palabra1:resto)
    | esDescifrado palabra2 palabra1 = (palabra1,palabra2) : todosLosParesDescifrados (palabra1:resto)
    | otherwise = todosLosParesDescifrados (palabra1:resto)

-- II. Codigo Vigenere 
-- EJ 11
expandirClave :: String -> Int -> String
expandirClave _ 0 = []
expandirClave letras n = expandirClaveAux letras (longitud letras) 0 n

expandirClaveAux :: String -> Int -> Int -> Int -> String
expandirClaveAux letras cantidad i n
    | i == n = ""
    | otherwise = nEsimoElemento letras (mod i cantidad) : expandirClaveAux letras cantidad (i+1) n

nEsimoElemento ::  (Eq t) => [t] -> Int -> t
nEsimoElemento (primero:_) 0 = primero
nEsimoElemento (primero:restoLista) n = nEsimoElemento restoLista (n-1)

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere letras clave = cifrarVigenereAux letras (expandirClave clave (longitud letras))

cifrarVigenereAux :: String -> String -> String
cifrarVigenereAux [] _ = []
cifrarVigenereAux (letra1:restoLetras) (clave1:restoClave) = desplazar letra1 (letraANatural clave1) : cifrarVigenereAux restoLetras restoClave

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere letras clave = descifrarVigenereAux letras (expandirClave clave (longitud letras))

descifrarVigenereAux :: String -> String -> String
descifrarVigenereAux [] _ = []
descifrarVigenereAux (letra1:restoLetras) (clave1:restoClave) = desplazar letra1 (-(letraANatural clave1)) : descifrarVigenereAux restoLetras restoClave

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado palabra cifrados = cifradoDeMenorDistancia (agruparDistancias palabra cifrados) 

cifradoDeMenorDistancia :: [(String,Int)] -> String
cifradoDeMenorDistancia [(unicoCifrado,_)] = unicoCifrado
cifradoDeMenorDistancia ((cifrado1,distancia1):(cifrado2,distancia2):restoPares)
    | distancia1 < distancia2 = cifradoDeMenorDistancia ((cifrado2,distancia2):restoPares)
    | otherwise = cifradoDeMenorDistancia ((cifrado1,distancia1):restoPares)

agruparDistancias :: String -> [String] -> [(String,Int)]
agruparDistancias _  [] = []
agruparDistancias palabra (cifrado1:restoCifrados) = (cifrado1,distancia palabra cifrado1) : agruparDistancias palabra restoCifrados

distancia :: String -> String -> Int
distancia _ [] = 0
distancia [] _ = 0
distancia (letra1:restoLetras) (cifrado1:restoCifrado) = absoluto (letraANatural letra1 - letraANatural cifrado1) + distancia restoLetras restoCifrado

absoluto :: Int -> Int
absoluto n
    | n < 0 = (-n)
    | otherwise = n

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (msj1:restoMsjs) claves cifrado = combinacionesVigenereAux msj1 claves cifrado ++ combinacionesVigenere restoMsjs claves cifrado

combinacionesVigenereAux :: String -> [String] -> String -> [(String, String)]
combinacionesVigenereAux _ [] _ = []
combinacionesVigenereAux msj (clave1:restoClaves) cifrado = devolverParCifradoSiCumple msj clave1 cifrado ++ combinacionesVigenereAux msj restoClaves cifrado

devolverParCifradoSiCumple :: String -> String -> String -> [(String,String)]
devolverParCifradoSiCumple palabra clave cifrado
    | cifrarVigenere palabra clave == cifrado = [(palabra,clave)]
    | otherwise = []