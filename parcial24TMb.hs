module Parcial24TMb where
type Mercaderias = [String]
type Stock = [(String,Integer)]
type Precios = [(String,Float)]

-- Ejercicio 1
generarStock :: Mercaderias -> Stock
generarStock [] = []
generarStock productos =  actualizarStockExistente productos []

actualizarStockExistente :: Mercaderias -> Stock -> Stock
actualizarStockExistente [] stockPrevio = stockPrevio
actualizarStockExistente (producto1:restoProductos) stockPrevio = actualizarStockExistente restoProductos (agregarProducto producto1 stockPrevio)

agregarProducto :: String -> Stock -> Stock
agregarProducto producto [] = [(producto,1)]
agregarProducto producto ((producto1,stock1):restoStock)
    | producto == producto1 = (producto1,stock1+1):restoStock
    | otherwise = (producto1,stock1) : agregarProducto producto restoStock

-- Ejercicio 2
stockDeProducto :: Stock -> String -> Integer
stockDeProducto [] _ = 0
stockDeProducto ((producto1,stock1):restoStock) producto
    | producto1 == producto = stock1
    | otherwise = stockDeProducto restoStock producto

-- Ejercicio 3
dineroEnStock :: Stock -> Precios -> Float
dineroEnStock [] _ = 0
dineroEnStock ((producto1,stock1):restoStock) listaDePrecios = (fromIntegral stock1 * buscarPrecio listaDePrecios producto1) + dineroEnStock restoStock listaDePrecios

buscarPrecio :: Precios -> String -> Float
--buscarPrecio [] _ = 0 -- Innecesario, lista de precios siempre contiene precio
buscarPrecio ((producto1,precio1):restoPrecios) producto
    | producto1 == producto = precio1
    | otherwise =  buscarPrecio restoPrecios producto

-- Ejercicio 4
aplicarOferta :: Stock -> Precios -> Precios
aplicarOferta _ [] = []
aplicarOferta listaDeStock ((producto1,precio1):restoPrecios)
    | stockDeProducto listaDeStock producto1 > 10 = (producto1,precio1 * 0.80) : aplicarOferta listaDeStock restoPrecios
    | otherwise =  (producto1,precio1) : aplicarOferta listaDeStock restoPrecios
