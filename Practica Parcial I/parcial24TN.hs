module Parcial24TN where
-- Ejercicio 1
aproboMasDeNMaterias :: [([Char],[Integer])] -> [Char] -> Integer -> Bool
aproboMasDeNMaterias registro alumno n = materiasAprobadas (notasDe alumno registro) > n

notasDe :: [Char] -> [([Char],[Integer])] -> [Integer]
notasDe _ [] = []
notasDe alumno ((alumno1,notas1):restoRegistro)
    | alumno == alumno1 = notas1
    | otherwise = notasDe alumno restoRegistro

materiasAprobadas :: [Integer] -> Integer
materiasAprobadas [] = 0
materiasAprobadas (primera:restoMaterias)
    | primera >= 4 = 1 + materiasAprobadas restoMaterias
    | otherwise = materiasAprobadas restoMaterias

-- Ejercicio 2
buenosAlumnos :: [([Char],[Integer])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos ((alumno1,notas1):restoRegistro)
    | promedio notas1 >= 8 && noTieneAplazos notas1 = alumno1 : buenosAlumnos restoRegistro
    | otherwise = buenosAlumnos restoRegistro

promedio :: [Integer] -> Float
promedio notas = fromIntegral (suma notas) / fromIntegral (cantidad notas)

suma :: [Integer] -> Integer 
suma [] = 0
suma (primero:restoNumeros) = primero + suma restoNumeros

cantidad :: [a] -> Integer
cantidad [] = 0
cantidad (primero:restoLista) = 1 + cantidad restoLista

noTieneAplazos :: [Integer] -> Bool
noTieneAplazos [] = True
noTieneAplazos (nota1:restoNotas) = nota1 >= 4 && noTieneAplazos restoNotas

-- Ejercicio 3
mejorPromedio :: [([Char],[Integer])] -> [Char]
mejorPromedio registro = mejorPromedioAux (promedios registro)

mejorPromedioAux :: [([Char],Float)] -> [Char]
mejorPromedioAux [(unicoAlumno,_)] = unicoAlumno
mejorPromedioAux ((alumno1,promedio1):(alumno2,promedio2):restoPromedios)
    | promedio1 < promedio2 = mejorPromedioAux ((alumno2,promedio2):restoPromedios)
    | otherwise = mejorPromedioAux ((alumno1,promedio1):restoPromedios)

promedios :: [([Char],[Integer])] -> [([Char],Float)]
promedios [] = []
promedios ((alumno1,nota1):restoRegistro) = (alumno1,promedio nota1) : promedios restoRegistro

-- Ejercicio 4
seGraduoConHonores :: [([Char],[Integer])] -> Integer -> [Char] -> Bool
seGraduoConHonores registro cantMateriasCarrera alumno = alumnoSeGraduoConHonores (alumno,notasDe alumno registro) cantMateriasCarrera (promedioMasAlto registro)

alumnoSeGraduoConHonores :: ([Char],[Integer]) -> Integer -> Float -> Bool
alumnoSeGraduoConHonores (alumno,notas) cantMateriasCarrera promedioMasAlto = materiasAprobadas notas >= (cantMateriasCarrera-1) && esBuenAlumno notas && promedio notas > (promedioMasAlto-1)

esBuenAlumno :: [Integer] -> Bool
esBuenAlumno notas = promedio notas >= 8 && noTieneAplazos notas

promedioMasAlto :: [([Char],[Integer])] -> Float
promedioMasAlto registro = promedioMasAltoAux (promedios registro)

promedioMasAltoAux :: [([Char],Float)] -> Float
promedioMasAltoAux [(_,unicoPromedio)] = unicoPromedio
promedioMasAltoAux ((alumno1,promedio1):(alumno2,promedio2):restoPromedios)
    | promedio1 < promedio2 = promedioMasAltoAux ((alumno2,promedio2):restoPromedios)
    | otherwise = promedioMasAltoAux ((alumno1,promedio1):restoPromedios)