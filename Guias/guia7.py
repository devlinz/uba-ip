import math
import random
# 1
def pertenece (lista: list, elemento: int) -> bool:
    for i in range(len(lista)):
        if lista[i] == elemento:
            return True
    return False
# Pendiente: 2 formas distintas más

def divide_a_todos (dividendos: list[int], divisor: int) -> bool:
    for i in range(len(dividendos)):
        if dividendos[i] % divisor != 0:
            return False
    return True

def suma_total (sumandos: list[int]) -> int:
    suma: int = 0
    for i in range(len(sumandos)):
        suma += sumandos[i]
    return suma

def ordenados (numeros: list[int]) -> bool:
    for i in range(len(numeros)-1):
        if numeros[i] > numeros[i+1]:
            return False
    return True

def hayPalabraLarga (palabras: list[str]) -> bool:
    for i in range(len(palabras)):
        if len(palabras[i]) > 7:
            return True
    return False

def esPalindromo (texto: str) -> bool:
    longitud: int = len(texto)
    for i in range(math.floor(longitud/2)):
        if texto[i] != texto[longitud-1-i]:
            return False
    return True

def fortalezaDeContraseña (contraseña: str) -> str:
    longitud: int = len(contraseña)
    if longitud > 8 and tieneMinuscula (contraseña) and tieneMayuscula (contraseña) and tieneNumero (contraseña):
        return "VERDE"
    if longitud < 5:
        return "ROJO"
    return "AMARILLO"
        
# Auxiliares
def tieneMinuscula (texto: str) -> bool:
    for i in range(len(texto)):
        if pertenece("abcdefghijklmnopqrstuvwxyz",texto[i]):
            return True
    return False

def tieneMayuscula (texto: str) -> bool:
    for i in range(len(texto)):
        if pertenece("ABCDEFGHIJKLMNOPQRSTUVWXYZ",texto[i]):
            return True
    return False

def tieneNumero (texto: str) -> bool:
    for i in range(len(texto)):
        if pertenece("0123456789",texto[i]):
            return True
    return False
#

def calcularBalance(movimientos: list[(str,int)]) -> int:
    balance: int = 0
    for i in range(len(movimientos)):
        tipoDeMovimiento = movimientos[i][0]
        if tipoDeMovimiento == "I":
            balance += movimientos[i][1]
            continue
        if tipoDeMovimiento == "R":
            balance -= movimientos[i][1]
            continue
    return balance

# Considerar mayúsculas? Como iguales?
def tiene3VocalesDistintas (texto: str) -> bool:
    vocales: str = "aeiou"
    cantidadDeVocales: int = 0
    for i in range(len(texto)):
        iesimoCaracter: str = texto[i]
        if pertenece (vocales, iesimoCaracter):
            quitarTodoCaracter (vocales, iesimoCaracter)
            cantidadDeVocales += 1
            if cantidadDeVocales >= 3:
                return True
    return False
# Auxiliares
def quitarTodoCaracter (texto: str, caracter: chr) -> str:
    textoNuevo: str = ""
    for i in range(len(texto)):
        iesimoCaracter: str = texto[i]
        if iesimoCaracter != caracter:
            textoNuevo += iesimoCaracter
    return textoNuevo
#

# Ejercicio 2
def anularPares (lista: list[int]) -> None:
    i: int = 0
    while (i < len(lista)): 
        lista[i] = 0
        i += 2

def anularPares2 (lista: list[int]) -> list[int]:
    listaAnulada: list[int] = lista
    anularPares (listaAnulada)
    return listaAnulada

def quitarVocales (texto: str) -> str:
    vocales: str = "aeiouAEIOU"
    textoSinVocales: str = ""
    for i in range(len(texto)):
        if not pertenece (vocales, texto[i]):
            textoSinVocales += texto[i]
    return textoSinVocales

def reemplaza_vocales (texto: str) -> str:
    vocales: str = "aeiou"
    textoSinVocales: str = ""
    for i in range(len(texto)):
        if pertenece (vocales, texto[i]):
            textoSinVocales += '_'
        else:
            textoSinVocales += texto[i]
    return textoSinVocales

def dar_vuelta_str (texto: str) -> str:
    reverso: str = ""
    for i in range(len(texto)-1,-1,-1):
        reverso += texto[i]
    return reverso

def eliminar_repetidos (texto: str) -> str:
    aparecidos: str = ""
    textoSinRepeticiones: str = ""
    for i in range(len(texto)):
        caracter: chr = texto[i]
        if not pertenece (aparecidos, caracter):
            textoSinRepeticiones += caracter
            aparecidos +=  caracter
    return textoSinRepeticiones

# Ejercicio 3
def aprobado (notas: list[int]) -> int:
    todoAprobado: bool = todosMayoresOIguales (notas, 4)
    notaPromedio: float = promedio (notas)
    if todoAprobado and notaPromedio >= 4:
        if notaPromedio >= 7:
            return 1
        else:
            return 2
    else:
        return 3
# Auxiliares
def todosMayoresOIguales (numeros: list[int], comparador: int) -> bool:
    for i in range(len(numeros)):
        if numeros[i] < comparador:
            return False
    return True

def promedio (numeros: list[int]) -> float:
    suma: int = 0
    longitud: int = len(numeros)
    for i in range(longitud):
        suma += numeros[i]
    return suma / longitud
#

# Ejercicio 4
def ingresarNombres () -> list[str]:
    nombres: list[str] = []
    while (True):
        print("Ingresá un nombre a registrar o 'list' para salir:")
        ingresado = input()
        if ingresado != "listo":
            nombres.append(ingresado)
        else:
            return nombres

def simularOperaciones() -> list[(str,int)]:
    operaciones: list[(str,int)] = []
    while (True):
        print ("Ingresá el tipo de operación: 'C'argar, 'D'escontar o 'X' (salir)")
        operacion = input()
        if operacion == "X":
            return operaciones
        if operacion == "C" or operacion == "D":
            print ("Ahora, ingresá el monto de la operación:")
            monto: int = int(input())
            operaciones.append((operacion,monto))

def siete_y_medio() -> None:
    baraja: list[int] = [1,2,3,4,5,6,7,10,11,12]
    mano: list[int] = []
    suma: float = 0
    ultimaCarta: int
    eleccion: str

    while (True):
        ultimaCarta = random.choice(baraja)
        mano.append(ultimaCarta)
        if ultimaCarta < 10:
            suma += ultimaCarta
        else:
            suma += 0.5
        print("Agregaste una carta a tu mano")
        if suma > 7.5:
            print("Perdiste!")
            break

        eleccion = "" 
        while (eleccion != "sigo" and eleccion != "me planto"):
            print("Elegí: 'sigo' o 'me planto'")
            eleccion = input()
        if eleccion == 'me planto':
            print ("Ganaste!")
            break
    print(mano)
    print(suma)

def pertenece_a_cada_uno_version_1y2 (listas: list[list[int]], elemento: int, salida: list[bool]) -> None:
    salida = []
    for i in range(len(listas)):
        salida.append(pertenece(listas[i],elemento))

def es_matriz (matriz: list[list[int]]) -> bool:
    filas: int = len(matriz)
    if filas == 0:
        return False
    columnas: int = len(matriz[0])
    if columnas == 0:
        return False
    
    for i in range(1,filas):
        if not len(matriz[i]) == columnas:
            return False
    return True

def filas_ordenadas (matriz: list[list[int]]) -> bool:
    for i in range(len(matriz)):
        if not ordenados (matriz[i]):
            return False
    return True

# Pendiente: Arreglar
def punto5 (d: int, p: int) -> list[list[int]]:
    return elevarMatrizCuadradada (matrizCuadradadaAleatoria (d, 0, 9), p)

def matrizCuadradadaAleatoria (tamaño: int, minimo: int, maximo: int) -> list[list[int]]:
    matriz: list[list[int]] = []
    for i in range(tamaño):
        matriz.append([])
        for _ in range(tamaño):
            matriz[i].append(random.randint(minimo,maximo))
    return matriz

def elevarMatrizCuadradada (matriz: list[list[int]], p: int) -> list[list[int]]:
    matrizElevada = matriz
    tamaño: int =  len(matriz)
    for i in range(p):
        for fila in range (tamaño):
            for columna in range(tamaño):
                matrizElevada[fila][columna] = elevarMatrizCuadradadaAux (matriz, fila, columna)
    return matrizElevada

def elevarMatrizCuadradadaAux (matriz: list[list[int]], fila: int, columna: int) -> int:
    suma: int = 0
    for k in range(1, len(matriz)):
        suma += matriz[fila][k] * matriz[k][columna]
    return suma

# Auxiliar
def printMatriz(matriz: list[list[int]]) -> None:
    matrizATexto: str = ""
    for i in range(len(matriz)):
        matrizATexto += str(matriz[i]) + '\n'
    print (matrizATexto)
#

import numpy

matrix = numpy.random.randint(0,9,(5,5))
printMatriz(matrix)
matrix = matrix ** 2 # No se calcular multiplicacion de matrices ya
printMatriz(matrix)