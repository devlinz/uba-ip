from queue import LifoQueue as Pila
from queue import Queue as Cola
import random
# Archivos
# Ejercicio 1
# Aux
def obtener_lineas(nombre_archivo: str) -> list[str]:
    archivo = open(nombre_archivo)
    lineas = archivo.readlines()
    archivo.close()
    return lineas

# 1
def contar_lineas (nombre_archivo: str) -> int:
    lineas = obtener_lineas(nombre_archivo)
    return len(lineas)

#print(contar_lineas("test.txt"))

# 2
def existe_palabra (palabra: str, nombre_archivo: str) -> bool:
    lineas = obtener_lineas(nombre_archivo)
    for i in range(len(lineas)):
        if contiene_palabra(lineas[i],palabra):
            return True
    return False

def contiene_palabra (texto: str, palabra: str) -> bool:
    caracterNumero: int = 0
    for i in range(len(texto)):
        if texto[i] == palabra[caracterNumero]:
            caracterNumero += 1
            if caracterNumero == len(palabra):
                return True
        else:
            caracterNumero = 0
    return False

#print(existe_palabra("hola","test.txt"))

def cantidad_apariciones (nombre_archivo: str, palabra: str) -> int:
    lineas = obtener_lineas(nombre_archivo)
    cantidad_lineas = len(lineas)
    apariciones: int = 0
    for i in range(cantidad_lineas):
        if contiene_palabra(lineas[i],palabra):
            apariciones += 1
    return apariciones

#print(cantidad_apariciones("test.txt","hola"))

# Ejercicio 2
def clonar_sin_comentarios (nombre_archivo: str) -> None:
    lineas = obtener_lineas(nombre_archivo)
    cantidad_lineas = len(lineas)
    lineas_sin_comentarios: list[str] = []
    for i in range(cantidad_lineas):
        linea = lineas[i]
        if not es_comentario(linea):
            lineas_sin_comentarios.append(linea)
    nuevo_archivo = open(nombre_archivo + " clonado.txt",'w') # 'w' permite crear archivo
    nuevo_archivo.writelines(lineas_sin_comentarios)
    nuevo_archivo.close()

def es_comentario (linea: str) -> bool:
    caracteres_linea = len(linea)
    for c in range(caracteres_linea):
            if linea[c] == ' ':
                continue # Ver el siguiente caracter
            if linea[c] == '#':
                return True
            else:
                return False
            
def escribir_lineas(nombre_archivo: str, lineas: list[str]) -> None:
    archivo = open(nombre_archivo,'w') # 'w' permite crear archivo
    archivo.writelines(lineas)
    archivo.close()

            
#clonar_sin_comentarios("test.txt")

# Ejercicio 3
def invertir_lineas (nombre_archivo: str) -> None:
    lineas = obtener_lineas(nombre_archivo)
    cantidad_lineas = len(lineas)
    lineas_invertidas: list[str] = []
    for i in range(cantidad_lineas):
        lineas_invertidas.append(lineas[cantidad_lineas-1-i])
    archivo_reverso = open("reverso.txt",'w')
    archivo_reverso.writelines(lineas_invertidas)
    archivo_reverso.close()

#invertir_lineas("test.txt")

# Ejercicio 4
def agregar_frase_al_final(nombre_archivo: str, frase: str) -> None:
    lineas = obtener_lineas(nombre_archivo)
    lineas[-1] = lineas[-1] + '\n'
    lineas.append(frase)
    escribir_lineas(nombre_archivo,lineas)

#agregar_frase_al_final("test.txt","frase al final")

# Ejercicio 5
def agregar_frase_al_principio (nombre_archivo: str, frase: str) -> None:
    lineas = obtener_lineas(nombre_archivo)
    cantidad_lineas = len(lineas)
    frase += '\n'
    lineas_nuevas = [frase]
    for i in range(cantidad_lineas):
        lineas_nuevas.append(lineas[i])
    escribir_lineas(nombre_archivo,lineas_nuevas)

#agregar_frase_al_principio("test.txt","frase al principio")

# Ejercicio 6
# Pendiente 

# Ejercicio 7
# Pendiente

# Pilas
# Aux
def copiar_pila (p: Pila[int]) -> Pila[int]:
    inversa: Pila[int] = Pila()
    copiada: Pila[int] = Pila()
    while not p.empty():
        inversa.put(p.get())
    while not inversa.empty():
        elemento = inversa.get()
        p.put(elemento)
        copiada.put(elemento)
    return copiada

# Ejercicio 8

# Ejercicio 9

# Ejercicio 10
def buscar_el_maximo (p: Pila[int]) -> int:
    copia_pila = copiar_pila(p)
    # Minimo un elemento
    maximo_actual: int = copia_pila.get()
    comparar: int
    while not copia_pila.empty():
        comparar = copia_pila.get()
        if comparar > maximo_actual:
            maximo_actual = comparar
    return maximo_actual

p = Pila()
p.put(5)
p.put(10)
p.put(2)
p.put(1)
p.put(3)
#print(buscar_el_maximo(p))

# Ejercicio 11
def esta_bien_balanceada (formula: str) -> bool:
    cantidad_caracteres = len(formula)
    parentesis_pendientes: int = 0
    for c in range(cantidad_caracteres):
        caracter: chr = formula[c] 
        if caracter == '(':
            parentesis_pendientes += 1
            continue
        if caracter == ')':
            parentesis_pendientes -=1
            if parentesis_pendientes < 0:
                return False
    return parentesis_pendientes == 0

# print(esta_bien_balanceada("1+(2*3)"))
# print(esta_bien_balanceada("1+(2*3))"))
# print(esta_bien_balanceada("(1+(2*3)"))

# Ejercicio 12
# Requiere expresion bien formada
def resultado_operacion (n1: float, n2: float, operacion: str) -> float:
    if operacion == "+":
        return n1 + n2
    if operacion == "-":
        return n1 - n2
    if operacion == "*":
        return n1 * n2
    if operacion == "/":
        return n1 / n2

# Requiere expresion bien formada
def dividir_en_tokens (expresion: str) -> list[str]:
    tokens: list[str] = []
    token_actual = ""
    for c in expresion:
        if c ==  " ":
            tokens.append(token_actual)
            token_actual = ""
        else:
            token_actual += c
    if token_actual != "":
        tokens.append(token_actual)
    return tokens

#print(dividir_en_tokens("3 4 + 5 * 2 -"))

def evaluar_expresion (expresion: str) -> float:
    tokens = dividir_en_tokens (expresion)
    pila: Pila[float] = Pila()
    for t in tokens:
        if t == "+" or t == "-" or t == "*" or t == "/":
            n2 = pila.get()
            n1 = pila.get()
            pila.put(resultado_operacion(n1,n2,t))
        else:
            pila.put(float(t))
    return pila.get()

#print(evaluar_expresion("3 4 + 5 * 2 -"))

# Colas
# 13
def generar_nros_al_azar (cantidad: int, desde: int, hasta: int) -> Cola[int]:
    nros: Cola[int] = Cola()
    for _ in range(cantidad):
        nros.put(random.randint(desde,hasta))
    return nros
#print(generar_nros_al_azar(100, 0, 99).queue)

# 14
def copiar_cola (cola: Cola) -> Cola:
    aux: Cola = Cola()
    copia: Cola = Cola()
    while not cola.empty():
        elemento = cola.get()
        aux.put(elemento)
        copia.put(elemento)
    while not aux.empty():
        cola.put(aux.get())
    return copia()
    

def cantidad_elementos (c: Cola) -> int:
    copia = copiar_cola(c)
    longitud = 0
    while not copia.empty():
        longitud += 1
    return longitud

# 15
def buscar_el_maximo (c: Cola[int]) -> int:
    maximo_actual: int = c.get()
    comparar: int
    while not c.empty():
        comparar = c.get()
        if comparar > maximo_actual:
            maximo_actual = comparar
    return maximo_actual

# 16
# 1
def pertence (elemento: int, lista: list[int]) -> bool:
    for n in lista:
        if elemento == n:
            return True
    return False

def tomar_al_azar_sin_repetir (min: int, max: int, cantidad: int) -> list[int]:
    nros: list[int] = []
    for _ in range(cantidad):
        n = random.randint(min,max)
        while pertence (n, nros):
            n = random.randint(min,max)
            
        nros.append(n)
    return nros

def armar_secuencia_de_bingo() -> Cola[int]:
    nros = tomar_al_azar_sin_repetir(0,99,100)
    cola: Cola[int] = Cola()
    for i in range(len(nros)):
        cola.put(nros[i])
    return cola

def jugar_carton_de_bingo (carton: list[int], bolillero: Cola[int]) -> int:
    bolillas: int = 0
    tachados: int = 0
    while not bolillero.empty():
        bolilla = bolillero.get()
        bolillas += 1
        for i in range(len(carton)):
            if carton[i] == bolilla:
                tachados += 1
                break
        if tachados == len(carton):
            return bolillas
        
def probar_bingo() -> int:
    carton = tomar_al_azar_sin_repetir(0,99,12)
    secuencia = armar_secuencia_de_bingo()
    #print(carton)
    #print(secuencia.queue)
    return jugar_carton_de_bingo(carton,secuencia)

#print(probar_bingo())

# Experimento
def bingo_mas_rapido (partidas: int) -> int:
    minimo = 100
    for _ in range(partidas):
        jugadas_necesarias = probar_bingo()
        if jugadas_necesarias < minimo:
            minimo = jugadas_necesarias
    return minimo

#print(bingo_mas_rapido(1000))

# 17
