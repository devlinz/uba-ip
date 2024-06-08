def ind_nesima_aparicion (numeros: list[int], apariciones: int, elemento: int) -> int:
    cant_numeros = len(numeros)
    for indice in range(cant_numeros):
        if numeros[indice] == elemento:
            apariciones -= 1
        if apariciones == 0:
            return indice

def mezclar (numeros1: list[int], numeros2: list[int]) -> list[int]:
    mezclados: list[int] = []
    longitud_listas = len(numeros1)
    for indice in range(longitud_listas):
        mezclados.append(numeros1[indice])
        mezclados.append(numeros2[indice])
    return mezclados

def matriz_capicua (matriz: list[list[int]]) -> bool:
    for fila in matriz:
        if not es_capicua(fila):
            return False
    return True

def es_capicua (numeros: list[int]) -> bool:
    media_longitud: int = len(numeros)//2
    for indice in range(media_longitud):
        if numeros[indice] != numeros[media_longitud+indice]:
            return False
    return True