import Text.Parsec (sysUnExpectError)
import Data.Text.Unsafe (iter)
import GHC.ResponseFile (escapeArgs)
import Text.Read.Lex (Number)
-- 1
fibonacci :: Integer -> Integer
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
esDivisible  :: Integer -> Integer -> Bool
esDivisible x 0 = False
esDivisible x y = resto x y == 0

resto :: Integer -> Integer -> Integer
resto x 0 = -1
resto x y
    | x < y = x
    | otherwise = resto (x - y) y

-- 4
sumaImpares :: Integer -> Integer
sumaImpares x
    | x <= 1 = 1
    | mod x 2 == 0 = sumaImpares (x - 1)
    | otherwise = x + sumaImpares (x - 2)

-- 5
medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact x = x * medioFact (x - 2)

-- 6
sumaDigitos :: Integer -> Integer
sumaDigitos x  = sumarDigitosDesde x 1

sumarDigitosDesde :: Integer -> Integer -> Integer
sumarDigitosDesde x i
    | x < elevar 10 i = iesimoDigito x i
    | otherwise = iesimoDigito x i + sumarDigitosDesde x (i+1)

-- 7
todosDigitosIguales :: Integer ->  Bool
todosDigitosIguales x = iterarDigitosIguales x 1

iterarDigitosIguales :: Integer -> Integer -> Bool
iterarDigitosIguales x i
    |  x < elevar 10 i = iesimoDigito x i == iesimoDigito x 1
    | otherwise = iesimoDigito x i == iesimoDigito x 1 && iterarDigitosIguales x (i+1)

-- 8
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito x n = div (mod x (elevar 10 n) - mod x (elevar 10 (n-1))) (elevar 10 (n-1))

cantDigitos :: Integer -> Integer
cantDigitos x = contarDigitosDesde x 1

contarDigitosDesde :: Integer -> Integer -> Integer
contarDigitosDesde x i
    | x < elevar 10 i = i
    | otherwise = contarDigitosDesde x (i+1)

elevar :: Integer -> Integer -> Integer
elevar _ 0 = 1
elevar x 1 = x
elevar x y
    | y < 0 = 0
    | otherwise = x * elevar x (y-1)

-- 9
-- Un numero es capicua cuando sus digitos se leen igual de ambos extremos
esCapicua :: Integer -> Bool
esCapicua x = iterarCapicua x 1

iterarCapicua :: Integer -> Integer -> Bool
iterarCapicua x i
    | i > div (cantDigitos x) 2 = True
    | otherwise = parEsCapicua x i && iterarCapicua x (i+1)

parEsCapicua x i = iesimoDigito x i == iesimoDigito x (cantDigitos x - i + 1)

-- 10
f1 :: Integer -> Integer
f1 1 = 2
f1 n = elevar 2 n + f1 (n-1)

f2 :: Integer -> Float -> Float
f2 1 q = q
f2 n q = powFloat q n + f2 (n-1) q

powFloat :: Float -> Integer -> Float
powFloat x 0 = 1
powFloat x y = x * powFloat x (y-1)

f3 :: Integer -> Float -> Float
f3 n q = f2 (2*n) q

f4 :: Integer -> Float -> Float
f4 n q = f3 n q - f2 (n-1) q

-- 11
eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = 1 / fromIntegral (factorial n) + eAprox (n-1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

-- 12
a :: Integer -> Float
a 1 = 2
a n = 2 + 1 / (a (n-1))

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = a n - 1

-- 13
f :: Integer -> Integer -> Integer
f 1 m = fAux 1 m
f n m = fAux n m + f (n-1) m

fAux :: Integer -> Integer -> Integer
fAux n 1 = 1
fAux n m =  elevar n m + f n (m-1)

-- 14
sumaPotencias :: Integer -> Integer -> Integer -> Integer 
sumaPotencias q 1 m = sumaPotenciasAux q 1 m
sumaPotencias q n m = sumaPotenciasAux q n m + sumaPotencias q (n-1) m

sumaPotenciasAux :: Integer -> Integer -> Integer -> Integer 
sumaPotenciasAux q n 1 = elevar q (n+1)
sumaPotenciasAux q n m = elevar q (n+m) + sumaPotenciasAux q n (m-1)

-- 15
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 1 m = sumaRacionalesAux 1 m
sumaRacionales n m = sumaRacionalesAux n m + sumaRacionales (n-1) m

sumaRacionalesAux :: Integer -> Integer -> Float
sumaRacionalesAux n 1 = fromIntegral n
sumaRacionalesAux n m = fromIntegral n / fromIntegral m + sumaRacionalesAux n (m-1)

-- 16
-- a
menorDivisor :: Integer ->  Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n i
    | n < i = n
    | mod n i == 0 = i
    | otherwise = menorDivisorDesde n (i+1)

-- b
esPrimo :: Integer -> Bool
esPrimo 1 = True
esPrimo n
    | n > 1 = menorDivisor n == n
    | otherwise = esPrimo (0 - n)

-- c
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos x y = mismoMenorDivisorDesde x y 2

mismoMenorDivisorDesde :: Integer -> Integer -> Integer -> Bool
mismoMenorDivisorDesde x y i
    | menorDivisorDesde x i == menorDivisorDesde y i = True
    | menorDivisorDesde x i == x && menorDivisorDesde y i == y = False
    | otherwise = mismoMenorDivisorDesde x y (mayorEntre (menorDivisorDesde x i) (menorDivisorDesde y i)) 

mayorEntre :: Integer -> Integer -> Integer
mayorEntre x y
    | x < y = y
    | otherwise = x

-- d
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoDesde n 2

nEsimoPrimoDesde :: Integer -> Integer -> Integer
nEsimoPrimoDesde n x
    | esPrimo x && n == 1 = x
    | esPrimo x = nEsimoPrimoDesde (n-1) (x+1)
    | otherwise = nEsimoPrimoDesde n (x+1)

-- 17
esFibonacci :: Integer -> Bool
esFibonacci n = esFibonacciDesde n 0

esFibonacciDesde :: Integer -> Integer -> Bool
esFibonacciDesde n i
    | n == fibonacci i = True
    | n < fibonacci i = False
    | otherwise = esFibonacciDesde n (i+1)

-- 18
mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n = mayorDigitoParComparado n (-1)

mayorDigitoParComparado :: Integer -> Integer -> Integer
mayorDigitoParComparado n d
    | n < 0 = mayorDigitoParComparado (-n) d
    | n == 0 = d
    | mod (iesimoDigito n 1) 2 /= 0 = mayorDigitoParComparado (div n 10) d
    | iesimoDigito n 1 <= d = mayorDigitoParComparado (div n 10) d
    | iesimoDigito n 1 > d = mayorDigitoParComparado (div n 10) (iesimoDigito n 1)

primerDigito :: Integer -> Integer
primerDigito n = mod n 10

-- 19
esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesdeHasta n 1 100

esSumaInicialDePrimosDesdeHasta :: Integer -> Integer -> Integer -> Bool
esSumaInicialDePrimosDesdeHasta n i m
    | n == nEsimoPrimo i = True
    | i == m = False
    | n < nEsimoPrimo i = False
    | otherwise = esSumaInicialDePrimosDesdeHasta (n - nEsimoPrimo i) (i+1) m

-- 20
-- Pendiente