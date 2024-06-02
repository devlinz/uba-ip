from queue import LifoQueue as Pila
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
