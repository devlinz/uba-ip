-- Ejercicio 1
hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar c [] = False
hayQueCodificar c ((c1,_):restoTuplas) = c == c1 || hayQueCodificar c restoTuplas

-- Ejercicio 2
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Integer
cuantasVecesHayQueCodificar c frase mapeo
    | hayQueCodificar c mapeo = contarApariciones c frase
    | otherwise = 0

contarApariciones :: Char -> [Char] -> Integer
contarApariciones _ [] = 0
contarApariciones c (c1:restoFrase)
    | c == c1 = 1 + contarApariciones c restoFrase
    | otherwise = contarApariciones c restoFrase

-- Ejercicio 3
laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar frase mapeo = caracterConMaximo (cuantasVecesHayQueCodificarTodos frase mapeo)

cuantasVecesHayQueCodificarTodos :: [Char] -> [(Char,Char)] -> [(Char,Integer)]
cuantasVecesHayQueCodificarTodos _ [] = []
cuantasVecesHayQueCodificarTodos frase ((c1,c2):restoMapeo) = (c1,cuantasVecesHayQueCodificar c1 frase ((c1,c2):restoMapeo)) : cuantasVecesHayQueCodificarTodos frase restoMapeo

caracterConMaximo :: [(Char,Integer)] -> Char
caracterConMaximo [(c,_)] = c
caracterConMaximo ((c1,n1):(c2,n2):restoTuplas)
    | n1 < n2 = caracterConMaximo ((c2,n2):restoTuplas)
    | otherwise = caracterConMaximo ((c1,n1):restoTuplas)

-- Ejercicio 4
codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase "" _ = ""
codificarFrase (c1:restoFrase) mapeo = mapear c1 mapeo : codificarFrase restoFrase mapeo

mapear :: Char -> [(Char,Char)] -> Char
mapear caracter [] = caracter
mapear caracter ((reemplazado,reemplazador):restoMapeo)
    | caracter == reemplazado = reemplazador
    | otherwise = mapear caracter restoMapeo

-- Testing
testEj4_normal = codificarFrase "hola" mapeoHolaAChau -- chau
testEj4_otro = codificarFrase "hola mundo" mapeoHolaAChau -- chau mundh
testEj4_igual = codificarFrase "gggg" mapeoHolaAChau -- gggg
testEj4_vacio = codificarFrase "hola" [] -- hola

testEj3_cumpleuno = laQueMasHayQueCodificar "hola" mapeoHolaAChau -- h
testEj3_cumplenvarios = laQueMasHayQueCodificar "hola mundo" mapeoHolaAChau -- o

testEj2_multiple = cuantasVecesHayQueCodificar 'o' "hola mundo" mapeoHolaAChau -- 2
testEj2_nomapea = cuantasVecesHayQueCodificar 'g' "gola" mapeoHolaAChau -- 0

testEj1_hayque = hayQueCodificar 'h' mapeoHolaAChau -- True
testEj1_nohayque = hayQueCodificar 'g' mapeoHolaAChau -- False
testEj1_vacio = hayQueCodificar ' ' [] -- False

-- Auxiliares
mapeoHolaAChau = [('h','c'),('o','h'),('l','a'),('a','u')]

