-- Ejercicio 2
-- a
absoluto :: Int -> Int
absoluto n
    | n < 0 = -n
    | otherwise = n

-- b
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto n m
    | absoluto n > absoluto m = n
    | otherwise = m

-- c
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z
    | x >= y && x >= z = x
    | y >= z = y
    | otherwise = z

-- d
algunoEs0 :: Int -> Int -> Bool
algunoEs0 0 _ = True
algunoEs0 _ 0 = True
algunoEs0 x y = False
--algunoEs0 x y = x == 0 || y == 0

-- e
ambosSon0 :: Int -> Int -> Bool
ambosSon0 0 0 = True
ambosSon0 x y = False
--ambosSon0 x y = x == y && y == 0

-- f
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y
    | x <= 3 = y <= 3
    | x > 7 = y > 7
    | otherwise = y > 3 && y <= 7

-- g
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z
    | x == y && y == z = x
    | x == y && y /= z = y+z
    | x /= y && y == z = x+y
    | otherwise = x+y+z

-- h
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

-- i
digitoUnidades :: Int -> Int
digitoUnidades n = mod n 10

-- j
digitoDecenas :: Int -> Int
digitoDecenas n = div (mod n 100) 10