def ultima_aparicion (numeros: list[int], numero: int) -> int:
    indice_aparicion: int = -1
    for indice in range(len(numeros)):
        if numeros[indice] == numero:
            indice_aparicion = indice
    return indice_aparicion

def elementos_exclusivos (numeros1: list[int], numeros2: list[int]) -> list[int]:
    diferencia_simetrica: list[int] = []
    for numero in numeros1:
        if not numero in numeros2:
            diferencia_simetrica.append(numero)
    for numero in numeros2:
        if not numero in numeros1:
            diferencia_simetrica.append(numero)
    return diferencia_simetrica

def contar_traducciones_iguales (traducciones1: dict[str,str], traducciones2: dict[str,str]) -> int:
    if len(traducciones2.keys()) > len(traducciones1.keys()):
        # Me aseguro que la primera lista tenga la cantidad de traducciones mas grandes
        return contar_traducciones_iguales (traducciones2,traducciones1)
    traducciones_iguales: int = 0
    for original,tradducion in traducciones1.items():
        if original in traducciones2.keys() and tradducion == traducciones2[original]:
            traducciones_iguales += 1
    return traducciones_iguales

def convertir_a_diccionario (numeros: list[int]) -> dict[int,int]:
    apariciones: dict[int,int] = dict()
    for numero in numeros:
        if not numero in dict.keys():
            apariciones[numero] = 1
        else:
            apariciones[numero] += 1
    return apariciones