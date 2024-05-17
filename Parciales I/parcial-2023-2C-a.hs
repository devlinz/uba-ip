{-
¡Vamos Campeones!
    En exactas se está jugando un torneo de fútbol y la facultad le pidió a los alumnos de IP programar algunas
funcionalidades en Haskell, Los datos con los que contamos para esto son los nombres de los equipos que participan
del torneo, los nombres de los arqueros titulares de cada uno de dichos equipos, y la cantidad de goles recibidos
por esos arqueros. Los nombres de los equipos y sus respectivos arqueros serán modelados mediante tuplas de tipo
(String, String), donde la primera componente representa el nombre del equipo, y la segunda representa el nombre
del arquero titular de dicho equipo.
    En los problemas en los cuales se reciben como parámetros secuencias arquerosPorEquipo y goles, cada posición de
la lista goles representará la cantidad de goles recibidos por el arquero del equipo que se encuentra en esa misma
posicion de arquerosPorEquipo. Por ejemplo, si la lista arquerosPorEquipo es [("Sacachispas", "Neyder Aragon"),
("Fenix", "Nahuel Galardi")] y la lista de goleses [3, 5], eso indicaría que Neyder Aragon recibió 3 goles, y
Nahuel Galardi 5.

Se pueden usar las siguientes funciones del preludio:
	- Listas: head, tail, last, init, length, elem, ++
	- Tuplas: fst, snd
	- Operaciones Lógicas: &&, ||, not
	- Constructores de listas: (x:xs), []
	- Constructores de tuplas: (x, y)
-}

{-
1) Atajaron Suplentes
problema atajaronSuplentes (arquerosPorEquipo: seq<String X String>, goles: seq<Z>, totalGolesTorneo: Z): Z {
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
	asegura: {
	res es la cantidad de goles recibidos en el torneo por arqueros que no son titulares en sus equipos.
	}
-}

atajaronSuplentes :: [(String, String)] -> [Integer] -> Integer -> Integer
atajaronSuplentes _ [] golesTotales  = golesTotales
atajaronSuplentes (primerEquipo:restoEquipos) (golesPrimero:restoGoles) golesTotales = atajaronSuplentes restoEquipos restoGoles (golesTotales-golesPrimero)

{-
2) Equipos Válidos
problema equiposValidos (arquerosPorEquipo: seq<String X String>): Bool {
	requiere: {True}
	asegura: {
	(res = True) <=> arquerosPorEquipo no contiene nombres de clubes repetidos, ni arqueros repetidos, ni jugadores con nombre del club
	}
-}

equiposValidos :: [(String, String)] -> Bool
equiposValidos [] = True
equiposValidos (primero:restoEquipos)
    | ambosComponentesIguales primero || repiteAlgunComponente primero restoEquipos = False
    | otherwise = equiposValidos restoEquipos

repiteAlgunComponente :: (Eq tipo1, Eq tipo2) => (tipo1, tipo2) -> [(tipo1, tipo2)] -> Bool
repiteAlgunComponente _ [] = False
repiteAlgunComponente (ejemplo1,ejemplo2) ((primero1,primero2):restoDuplas) = ejemplo1 == primero1 || ejemplo2 == primero2 || repiteAlgunComponente (ejemplo1,ejemplo2) restoDuplas

ambosComponentesIguales :: (Eq tipo) => (tipo, tipo) -> Bool
ambosComponentesIguales (primero,segundo) = primero == segundo

{-
3) Porcentaje de goles
problema porcentajeDeGoles (arquero: String, arquerosPorEquipo: seq<String X String>, goles: seq<Z>): R {
	requiere: {La segunda componente de algún elemento de arquerosPorEquipo es arquero}
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {Hay al menos un elemento de goles mayores estricto a 0}
	asegura: {
	res es el porcentaje de goles que recibió arquero sobre el total de goles recibidos por arqueros titulares
	}
}

Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como float la división entre dos
numeros de tipo Integer.
-}
division :: Integer -> Integer -> Float
division a b = fromIntegral a / fromIntegral b

porcentajeDeGoles :: String -> [(String, String)] -> [Integer] -> Float
porcentajeDeGoles arquero arquerosPorEquipo golesAtajados =  100.0 * division (golesDeArquero arquero arquerosPorEquipo golesAtajados) (sumar golesAtajados)

golesDeArquero :: String -> [(String, String)] -> [Integer] -> Integer
golesDeArquero arqueroBuscado ((_,arquero):restoEquipos) (golesAtajados:restoGolesAtajados)
    | arqueroBuscado == arquero = golesAtajados
    | otherwise = golesDeArquero arqueroBuscado restoEquipos restoGolesAtajados

sumar :: (Num tipo) => [tipo] -> tipo
sumar [] = 0
sumar (primero:restoNumeros) = primero + sumar restoNumeros

{-
4) Valla Menos Vencida
problema vallaMenosVencida (arquerosPorEquipo: seq<String X String>, goles: seq<Z>): String {
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {|goles| > 0}
	asegura: {
	res es alguno de los arqueros de arquerosPorEquipo que menor goles recibió de acuerdo a goles
	}
-}

vallaMenosVencida :: [(String, String)] -> [Integer] -> String
vallaMenosVencida [(_,arquero)] [golesDelArquero] = arquero
vallaMenosVencida ((equipo1,arquero1):(equipo2,arquero2):restoEquipos) (golesDelArquero1:golesDelArquero2:restoGoles)
    | golesDelArquero1 > golesDelArquero2 = vallaMenosVencida ((equipo2,arquero2):restoEquipos) (golesDelArquero2:restoGoles)
    | otherwise = vallaMenosVencida ((equipo1,arquero1):restoEquipos) (golesDelArquero1:restoGoles)

-- Testing
test1_12 = atajaronSuplentes [("Equipo1","Arquero1"),("Equpo2","Arquero2"),("Equipo3","Arquero3")] [1,7,5] 25
-- Espero 25-1-7-5 = 12

test2_valido = equiposValidos [("Equipo1","Arquero1"),("Equpo2","Arquero2"),("Equipo3","Arquero3")]
-- Espero True
test2_equipoRepetido = equiposValidos [("Equipo1","Arquero1"),("Equipo2","Arquero2"),("Equipo1","Arquero3")]
test2_arqueroRepetido = equiposValidos [("Equipo1","Arquero1"),("Equipo2","Arquero2"),("Equipo3","Arquero2")]
test2_mismoNombre = equiposValidos [("Equipo1","Arquero2"),("Equipo2","Equipo2")]
-- Espero False

test3_estandar = porcentajeDeGoles "Arquero2" [("Equipo1","Arquero1"),("Equpo2","Arquero2"),("Equipo3","Arquero3")] [1,7,5]
-- Espero 7 / (1+7+5) = 53%

test4_estandar = vallaMenosVencida [("Equipo1","Arquero1"),("Equpo2","Arquero2"),("Equipo3","Arquero3")] [1,7,5]
test4_empatados = vallaMenosVencida [("Equipo1","Arquero1"),("Equpo2","Arquero2"),("Equipo3","Arquero3")] [7,5,5]