import Parcial24TMb
import Test.HUnit
import GHC.Generics (prec)

mercaderiaSinRepetir = ["Banana","Manzana","Naranja","Pera","Frutilla"]
mercaderiaConDescuento = ["Banana","Manzana","Manzana","Naranja","Banana","Pera","Banana","Banana","Banana","Banana","Banana","Banana","Banana","Banana","Banana"]
stockSinRepetir = [("Banana",1),("Manzana",1),("Naranja",1),("Pera",1),("Frutilla",1)]
stockConDescuento = [("Banana",11),("Manzana",2),("Naranja",1),("Pera",1)]
preciosEjemplo :: Precios
preciosEjemplo = [("Banana",2.5),("Manzana",10.0),("Naranja",5.0),("Pera",7.5),("Frutilla",6.5)]

testear = runTestTT tests
tests = test [
    "1. Uno de cada" ~: generarStock mercaderiaSinRepetir ~?= stockSinRepetir,
    "1. Vacio" ~:       generarStock [] ~?= [],
    "1. Repetidos" ~:   generarStock mercaderiaConDescuento ~?= stockConDescuento,
    "2. Hay uno" ~:     stockDeProducto stockConDescuento "Pera" ~?= 1,
    "2. Hay varios" ~:  stockDeProducto stockConDescuento "Banana" ~?= 11,
    "2. No aparece" ~:  stockDeProducto stockConDescuento "Frutilla" ~?= 0,
    "3. Sin stock" ~:   dineroEnStock [] preciosEjemplo ~?= 0,
    "3. Uno de cada" ~: dineroEnStock stockSinRepetir preciosEjemplo ~?= 31.5,
    "3. Repetidos" ~:   dineroEnStock stockConDescuento preciosEjemplo ~?= 60,
    "4. No aplica" ~:   aplicarOferta stockSinRepetir preciosEjemplo ~?= preciosEjemplo,
    "4. Si aplica" ~:   aplicarOferta stockConDescuento preciosEjemplo ~?= ("Banana",2.0) : tail preciosEjemplo,
    "4. Vacio" ~:       aplicarOferta [] preciosEjemplo ~?= preciosEjemplo
    ]