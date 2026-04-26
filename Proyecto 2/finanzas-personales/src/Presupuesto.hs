module Presupuesto where

import Types



-- =========================================================
-- MENÚ PRINCIPAL DE PRESUPUESTOS
-- =========================================================

menuPresupuestos :: EstadoSistema -> IO EstadoSistema  -- recibe un estado y lo devuelve actualizado
menuPresupuestos estado = do
    putStrLn "\n=== MENÚ DE PRESUPUESTOS ==="
    putStrLn "1. Definir presupuesto"
    putStrLn "2. Volver"
    putStrLn "Seleccione una opción:"
    
    opcion <- getLine --guarda la opcion 

    case opcion of
        "1" -> do
            estadoActualizado <- definirPresupuestoIO estado -- guarda el estado ya actualizado que devuelve
            menuPresupuestos estadoActualizado -- vuelve a mostrar el menu
        
        "2" -> return estado -- devuelve el estado actual 
        
        _ -> do -- cualquier otro 
            putStrLn "Opción inválida"
            menuPresupuestos estado


-- funcion para definir el presupuesto 

definirPresupuestoIO :: EstadoSistema -> IO EstadoSistema
definirPresupuestoIO estado = do
    putStrLn "\n--- Definir nuevo presupuesto ---"
    -- cada funcion pide un dato y lo devuelve con el tipo de dato correcto
    categoria <- leerCategoria
    monto     <- leerMonto
    mes       <- leerMes
    anio      <- leerAnio

    let nuevo = crearPresupuesto categoria monto mes anio  -- crea el objeto
    let estadoNuevo = agregarPresupuesto nuevo estado -- crea uno nuevo con el presupuesto agregado

    putStrLn " Presupuesto agregado correctamente"
    return estadoNuevo -- devuelve el nuevo estado


-- FUNCIONES auxiliares

-- Crear presupuesto
crearPresupuesto :: String -> Double -> Int -> Int -> Presupuesto  -- construye el dato
crearPresupuesto cat monto mes anio =
    Presupuesto cat monto mes anio


-- Agregar presupuesto al estado
agregarPresupuesto :: Presupuesto -> EstadoSistema -> EstadoSistema
agregarPresupuesto p estado =
    estado {
        presupuestos = p : presupuestos estado -- agrega el presupuesto a la lista 
    }


-- =========================================================
-- VALIDACIÓN
-- =========================================================

-- Verifica si un string es número
esNumero :: String -> Bool
esNumero s = case reads s :: [(Double, String)] of -- intenta convertir a numero
    [(n, "")] -> True -- su se pudo convertir 
    _         -> False


leerCategoria :: IO String
leerCategoria = do
    putStrLn "Ingrese la categoria:"
    categoria <- getLine
    if null categoria --verifica si esta vacia 
        then do
            putStrLn " La categoria no puede estar vacia"
            leerCategoria -- si hay un error se vuelve a llamar a si misma 
        else return categoria


leerMonto :: IO Double
leerMonto = do
    putStrLn "Ingrese el monto máximo:"
    montoStr <- getLine
    if not (esNumero montoStr)
        then do
            putStrLn "Monto inválido"
            leerMonto
        else return (read montoStr) -- convierte a nuemero 

leerMes :: IO Int
leerMes = do
    putStrLn "Ingrese el mes (1-12):"
    mesStr <- getLine
    if not (esNumero mesStr)
        then do
            putStrLn "Mes inválido"
            leerMes
        else do
            let mes = read mesStr -- lo convierto a entero 
            if mes < 1 || mes > 12
                then do
                    putStrLn "El mes debe estar entre 1 y 12"
                    leerMes
                else return mes


leerAnio :: IO Int
leerAnio = do
    putStrLn "Ingrese el año:"
    anioStr <- getLine
    if not (esNumero anioStr)
        then do
            putStrLn "Año inválido"
            leerAnio
        else do
            let anio = read anioStr
            if anio <= 0
                then do
                    putStrLn "Año inválido"
                    leerAnio
                else return anio
