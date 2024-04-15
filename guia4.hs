import Text.Parsec (sysUnExpectError)
import Data.Text.Unsafe (iter)
import GHC.ResponseFile (escapeArgs)
-- 1
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

-- 2
parteEntera :: Float -> Int
parteEntera x = -1

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
    | x < elevar 10 i = iesimoDigito x i == iesimoDigito x 1
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

-- 10 : En proceso
f1 :: Int -> Int
f1 n = f1Iter 0 n

f1Iter :: Int -> Int -> Int
f1Iter i n
    | i == n = elevar 2 i
    | otherwise = elevar 2 i + f1Iter (i+1) n