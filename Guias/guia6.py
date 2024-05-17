import math

# Ejercicio 1
def imprimir_hola_mundo () -> None:
    print("¡Hola mundo!")

def imprimir_un_verso () -> None:
    print("Jigsaw falling into place\nSo there is nothing to explain\nYou eye each other as you pass\nShe looks back, you look back\nNot just once\nNot just twice\nWish away the nightmare\nWish away the nightmare\nYou've got a light, you can feel it on your back\nA light, you can feel it on your back\nJigsaw falling into place")

def raizDe2() -> float:
    return round (math.sqrt (2), 2)

def factorial_de_2 () -> int:
    return 2

def perimetro () -> float:
    return 2 * math.pi

# Ejercicio 2
def imprimir_saludo (nombre: str) -> None:
    print ("Hola " + nombre)

def raiz_cuadrada_de (numero: int) -> float:
    return math.sqrt (numero)

def fahrenheit_a_celsius (temp_far: int) -> int:
    return ((temp_far - 32) * 5) / 9

def imprimir_dos_veces (estribillo: str) -> None:
    print (estribillo * 2)

def es_multiplo_de (n: int, m: int) -> bool:
    return n % m == 0

def es_par (numero: int) -> bool:
    return es_multiplo_de (numero,2)

def cantidad_de_pizzas (comensales: int, min_cant_de_porciones: int) -> int:
    return math.ceil ((comensales * min_cant_de_porciones) / 8)

# Ejercicio 3
def alguno_es_0 (numero1: int, numero2: int) -> bool:
    return numero1 == 0 or numero2 == 0

def ambos_son_0 (numero1: int, numero2: int) -> bool:
    return numero1 == 0 and numero2 == 0

def es_nombre_largo (nombre: str) -> bool:
    return 3 <= len(nombre) and 8>= len(nombre)

def es_bisiesto (año: int) -> bool:
    return es_multiplo_de (año, 400) or (es_multiplo_de (año, 4) and not es_multiplo_de(año, 100))

# Ejercicio 4
def peso_pino (altura_en_cm: int) -> int:
    return min (altura_en_cm,300) * 3 + max (altura_en_cm - 300, 0) * 2

def es_peso_util (peso: int) -> bool: 
    return peso <= 1000 and peso >= 400

def sirve_pino (altura_en_cm: int) -> bool:
    return es_peso_util (peso_pino (altura_en_cm))

# Ejercicio 5
def devolver_el_doble_si_es_par (numero: int) -> int:
    if (es_par (numero)):
        return 2 * numero
    else:
        return numero

def devolver_valor_si_es_par_sino_el_que_sigue (numero: int) -> int:
    if (es_par (numero)):
        return numero
    else:
        return numero+1
    
def devolver_el_doble_si_es_multiplo3_el_triple_si_es_multiplo9 (numero: int) -> int:
    if (es_multiplo_de (numero, 3)):
        return 2*numero
    else:
        if (es_multiplo_de (numero, 9)):
            return 3*numero
        else:
            return numero
        
def lindo_nombre (nombre: str) -> None:
    if (len(nombre) >= 5):
        print("Tu nombre tiene muchas letras!")
    else:
        print("Tu nombre tiene menos de 5 caracteres")

def elRango (numero: int) -> None:
    if (numero < 5):
        print ("Menor a 5")
    else:
        if (numero > 20):
            print ("Mayor a 20")
        else:
            if (numero >= 10):
                print ("Entre 10 y 20")

def trabajas_o_no (sexo: chr, edad: int) -> None:
    if (edad < 18 or edad >= 65 or (edad >= 60 and sexo == 'F')):
        print ("Andá de vacaciones")
    else:
        print ("Te toca trabajar")

# Ejercicio 6
def imprimir_1_al_10 () -> None:
    x: int = 1
    while (x <= 10):
        print (x)
        x = x + 1

def imprimir_pares_10_al_40 () -> None:
    x: int = 10
    while (x <= 40):
        print (x)
        x = x + 2

def eco_10_veces () -> None:
    x: int = 1
    while (x < 10):
        print ("eco")
        x = x + 1

def cuenta_regresiva (inicio: int) -> None:
    while (inicio >= 1):
        print (inicio)
    print ("Despegue")

def monitorear_viaje (año: int, objetivo: int) -> None:
    while (año > objetivo):
        año = año - 1
        print("Viajo un año al pasado, estamos en el año: " + año)

def ir_con_aristoteles (año: int) -> None:
    while (año >= -364):
        año = año - 20
        print("Viajo 20 años al pasado, estamos en el año: " + año)
    # imprimir el último solamente si está más cerca que el anterior

def diferencia (n1: int, n2: int) -> int:
    return -1
