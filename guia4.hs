import Text.Parsec (sysUnExpectError)
import Data.Text.Unsafe (iter)
import GHC.ResponseFile (escapeArgs)
-- 1
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

-- 2
parteEntera :: Float -> Float
parteEntera x = x - parteFlotante x

parteFlotante :: Float -> Float
parteFlotante x
    | x < 1 = x
    | otherwise = parteFlotante (x-1)

-- 3
esDivisible  :: Int -> Int -> Bool
esDivisible x 0 = False
esDivisible x y = resto x y == 0

resto :: Int -> Int -> Int
resto x 0 = -1
resto x y
    | x < y = x
    | otherwise = resto (x - y) y

-- 4
sumaImpares :: Int -> Int
sumaImpares x
    | x <= 1 = 1
    | mod x 2 == 0 = sumaImpares (x - 1)
    | otherwise = x + sumaImpares (x - 2)

-- 5
medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact x = x * medioFact (x - 2)

-- 6
sumaDigitos :: Int -> Int
sumaDigitos x  = sumarDigitosDesde x 1

sumarDigitosDesde :: Int -> Int -> Int
sumarDigitosDesde x i
    | x < elevar 10 i = iesimoDigito x i
    | otherwise = iesimoDigito x i + sumarDigitosDesde x (i+1)

-- 7
todosDigitosIguales :: Int ->  Bool
todosDigitosIguales x = iterarDigitosIguales x 1

iterarDigitosIguales :: Int -> Int -> Bool
iterarDigitosIguales x i
    |  x < elevar 10 i = iesimoDigito x i == iesimoDigito x 1
    | otherwise = iesimoDigito x i == iesimoDigito x 1 && iterarDigitosIguales x (i+1)

-- 8
iesimoDigito :: Int -> Int -> Int
iesimoDigito x n = div (mod x (elevar 10 n) - mod x (elevar 10 (n-1))) (elevar 10 (n-1))

cantDigitos :: Int -> Int
cantDigitos x = contarDigitosDesde x 1

contarDigitosDesde :: Int -> Int -> Int
contarDigitosDesde x i
    | x < elevar 10 i = i
    | otherwise = contarDigitosDesde x (i+1)

elevar :: Int -> Int -> Int
elevar _ 0 = 1
elevar x 1 = x
elevar x y
    | y < 0 = 0
    | otherwise = x * elevar x (y-1)

-- 9
-- Un numero es capicua cuando sus digitos se leen igual de ambos extremos
esCapicua :: Int -> Bool
esCapicua x = iterarCapicua x 1

iterarCapicua :: Int -> Int -> Bool
iterarCapicua x i
    | i > div (cantDigitos x) 2 = True
    | otherwise = parEsCapicua x i && iterarCapicua x (i+1)

parEsCapicua x i = iesimoDigito x i == iesimoDigito x (cantDigitos x - i + 1)

-- 10
f1 :: Int -> Int
f1 1 = 2
f1 n = elevar 2 n + f1 (n-1)

f2 :: Int -> Float -> Float
f2 1 q = q
f2 n q = powFloat q n + f2 (n-1) q

powFloat :: Float -> Int -> Float
powFloat x 0 = 1
powFloat x y = x * powFloat x (y-1)

f3 :: Int -> Float -> Float
f3 n q = f2 (2*n) q

f4 :: Int -> Float -> Float
f4 n q = f3 n q - f2 (n-1) q

-- 11
eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = 1 / fromIntegral (factorial n) + eAprox (n-1)

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

-- 12
a :: Int -> Float
a 1 = 2
a n = 2 + 1 / (a (n-1))

raizDe2Aprox :: Int -> Float
raizDe2Aprox n = a n - 1

-- 13 
-- Pendiente
f :: Int -> Int -> Int
f n m = -1

