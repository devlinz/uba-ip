-- 4b
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d) = (esMenor a c) && (esMenor b d)

esMenor :: Float -> Float -> Bool
esMenor x y = x < y

-- 4f
posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (x,y,z) 
    | esPar x = 1
    | esPar y = 2
    | esPar z = 3
    | otherwise = 4

esPar :: Int -> Bool
esPar x = mod x 2 == 0

-- 6: Error bisiesto 1900 = False, bisiesto 2000 = True
bisiesto :: Int -> Bool
bisiesto x = (mod x 4 == 0) || ((mod x 100 == 0) || (mod x 400 /= 0))

-- 7
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a,b,c) (x,y,z) = absoluto (a-x) + absoluto (b-y) + absoluto (c-z)

absoluto :: Float -> Float
absoluto x 
    | x < 0.0 = -x
    | otherwise = x

-- 8
comparar :: Int -> Int -> Int
comparar x y
    | sumaUltimos2Digitos x > sumaUltimos2Digitos y = -1
    | sumaUltimos2Digitos x < sumaUltimos2Digitos y = 1
    | sumaUltimos2Digitos x == sumaUltimos2Digitos y = 0

comparar2 :: Int -> Int -> Int
comparar2 x y = (compararAux (sumaUltimos2Digitos x) (sumaUltimos2Digitos y))

compararAux :: Int -> Int -> Int
compararAux x y
    | x > y = -1
    | x < y = 1
    | x == y = 0

sumaUltimos2Digitos :: Int -> Int
sumaUltimos2Digitos x = digitoDecenas x + digitoUnidades x

digitoDecenas :: Int -> Int
digitoDecenas x = div((mod x 100) - (digitoUnidades x)) 10

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

-- 9
-- f4 = promedio
-- f5 = promedio par

