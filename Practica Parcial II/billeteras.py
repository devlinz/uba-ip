def verificar_transacciones (movimientos: str) -> int:
    saldo: int = 0
    for movimiento in movimientos:
        if movimiento == 'x':
            return saldo
        elif movimiento == 'r':
            saldo += 350
        elif movimiento == 'v':
            saldo -= 56
            if saldo < 0:
                return saldo + 56
    return saldo

def valor_minimo (temperaturas: list[tuple[float,float]]) -> float:
    minimo: float = temperaturas[0]
    for temperatura in temperaturas:
        if temperatura[0] < minimo:
            minimo = temperatura[0]
    return minimo

def valores_extremos (cotizaciones_diarias: dict[str,list[tuple[int,float]]]) -> dict[str,tuple[float,float]]:
    extremos: dict[str,tuple[float,float]] = dict()
    for compañia,valores_diarios in cotizaciones_diarias.items():
        minimo: float = valores_diarios[0][1]
        maximo: float = valores_diarios[0][1]
        for dia_valor in valores_diarios:
            valor = dia_valor[1]
            if valor < minimo:
                minimo = valor
            if valor > maximo:
                maximo = valor
        extremos[compañia] = (minimo,maximo)
    return extremos

