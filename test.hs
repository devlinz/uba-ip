import Parcial24TN
import Test.HUnit

main = runTestTT testear

testear = test [
    "1. De más" ~:      aproboMasDeNMaterias registroNormal "Primero" 2 ~?= True,
    "1. De menos" ~:    aproboMasDeNMaterias registroNormal "Segundo" 2 ~?= False,
    "1. De exacto" ~:   aproboMasDeNMaterias registroNormal "Tercero" 2 ~?= False,
    "2. Normal" ~:      buenosAlumnos registroNormal ~?= ["Primero","Quinto"],
    "2. Nadie" ~:       buenosAlumnos unSoloRegistro ~?= [],
    "3. Normal" ~:      mejorPromedio registroNormal ~?= "Quinto",
    "3. Mismo" ~:       mejorPromedio mismoPromedio ~?= "Primero",
    "4. Caso sí" ~:     seGraduoConHonores registroNormal 3 "Quinto" ~?= True,
    "4. Caso justo" ~:  seGraduoConHonores registroNormal 3 "Primero" ~?= True,
    "4. Caso no" ~:     seGraduoConHonores registroNormal 3 "Tercero" ~?= False
    ]

    
-- Testing
registroNormal = [("Primero",[10,7,8]),("Segundo",[2,5,7]),("Tercero",[1,3,9]),("Cuarto",[0,2,1]),("Quinto",[10,8,9])]
mismoPromedio = [("Primero",[10,7,8]),("Segundo",[2,5,7]),("Tercero",[7,8,10])]
unSoloRegistro = [("Unico",[2,5,7])]