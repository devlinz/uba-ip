def acomodar (boletas: list[str]) -> str:
    acomodadas: list[str] = [""] * len(boletas)
    up: int = 0
    lla: int = len(acomodadas)-1
    for boleta in boletas:
        if boleta == "UP":
            acomodadas[up] = boleta
            up += 1
        else:
            acomodadas[lla] = boleta
            lla -= 1
    return acomodadas

def pos_umbral (registros: list[int], umbral: int) -> int:
    entradas: int = 0
    for i in range(len(registros)):
        if registros[i] > 0:
            entradas += registros[i]
            if entradas > umbral:
                return i
    return -1

#print(pos_umbral([1,-2,0,5,-7,3],5))

def columnas_repetidas (matriz: list[list[int]]) -> bool:
    mitad_columnas: int = len(matriz[0])//2
    for columna in range(mitad_columnas):
        for fila in matriz:
            if fila[columna] != fila[mitad_columnas+columna]:
                return False
    return True

#print(columnas_repetidas([[1,2,1,2],[-5,6,-5,6],[0,1,0,1]]))

def cuenta_posiciones_por_nacion (naciones: list[str], torneos: dict[int,list[str]]) -> dict[str,list[int]]:
    posiciones_por_nacion: dict[str,list[int]] = dict()

    for nacion in naciones:
        posiciones_por_nacion[nacion] = [0] * len(naciones)

    for torneo in torneos.values():
        for posicion in range(len(torneo)):
            nacion_en_posicion = torneo[posicion]
            posiciones_por_nacion[nacion_en_posicion][posicion] += 1

    return posiciones_por_nacion 

#print(cuenta_posiciones_por_nacion(["arg", "aus", "nz", "sud"],{2023:["nz", "sud", "arg", "aus"],2022:["nz", "sud", "aus", "arg"]}))