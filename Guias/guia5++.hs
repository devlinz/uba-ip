-- Auxiliares
type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

elNombre :: Contacto -> Nombre
elNombre (nombre,_) = nombre

elTelefono :: Contacto -> Telefono
elTelefono (_,telefono) = telefono

-- Ejercicio 4
-- a
enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos nombre ((nombrePrimero,_):restoContactos) = nombre == nombrePrimero || enLosContactos nombre restoContactos

-- b
agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto contacto [] = [contacto]
agregarContacto (nombre,numero) ((nombrePrimero,numeroPrimero):restoContactos)
    | nombre == nombrePrimero = (nombre,numero):restoContactos
    | otherwise = (nombrePrimero,numeroPrimero):(agregarContacto (nombre,numero) restoContactos)

-- c
-- Pendiente
eliminarContacto :: Nombre -> ContactosTel -> ContactosTel
eliminarContacto nombre agenda = agenda