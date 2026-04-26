module Presupuesto where

import Types
import Data.ByteString (putStr)



-- =========================================================
-- MENÚ PRINCIPAL DE PRESUPUESTOS
-- =========================================================

menuPresupuestos :: EstadoSistema -> IO EstadoSistema  -- recibe un estado y lo devuelve actualizado
menuPresupuestos estado = do
    putStrLn "\n=== MENÚ DE PRESUPUESTOS ==="
    putStrLn "1. Definir presupuesto"
    putStrLn "2. Obtener informacion de un presupuesto"
    putStrLn "3. Volver"
    putStrLn "Seleccione una opción:"
    
    opcion <- getLine --guarda la opcion 

    case opcion of
        "1" -> do
            estadoActualizado <- definirPresupuestoIO estado -- guarda el estado ya actualizado que devuelve
            menuPresupuestos estadoActualizado -- vuelve a mostrar el menu
        "2" -> do
            obtenerPresupuesto estado
            menuPresupuestos estado
        
        "3" -> return estado -- devuelve el estado actual 
        
        _ -> do -- cualquier otro 
            putStrLn "Opción inválida"
            menuPresupuestos estado

-----------------------------------------------------------------------------------------------------------------------
-- funcion para definir el presupuesto 

definirPresupuestoIO :: EstadoSistema -> IO EstadoSistema
definirPresupuestoIO estado = do
    putStrLn "\n--- Definir nuevo presupuesto ---"

    categoria <- leerCategoria
    monto     <- leerMonto
    mes       <- leerMes
    anio      <- leerAnio

    let nuevo = crearPresupuesto categoria monto mes anio
    let (estadoNuevo, agregado) = agregarPresupuesto nuevo estado

    if agregado
        then do
            putStrLn "Presupuesto agregado correctamente"
            return estadoNuevo
        else do
            putStrLn " Ya existe un presupuesto para esa categoría en ese periodo"
            return estado


obtenerPresupuesto :: EstadoSistema -> IO ()
obtenerPresupuesto estado = do 
    putStrLn "\nIngrese la informacion del presupuesto"

    categoria <- leerCategoria
    mes       <- leerMes
    anio      <- leerAnio

    let resultado = buscarPresupuestoUnico categoria mes anio estado
    putStrLn  "\n"

    case resultado of
        Just p  -> putStrLn (mostrarPresupuesto p)
        Nothing -> putStrLn "No se encontró el presupuesto" 

---------------------------------------------------------------------------------------------------------------------------------
-- FUNCIONES auxiliares

-- Crear presupuesto
crearPresupuesto :: String -> Double -> Int -> Int -> Presupuesto  -- construye el dato
crearPresupuesto cat monto mes anio =
    Presupuesto cat monto mes anio


-- validar duplicados

existePresupuesto :: Presupuesto -> [Presupuesto] -> Bool
existePresupuesto nuevo lista =
    any (\p ->
        presupuestoCategoria p == presupuestoCategoria nuevo &&
        periodoMes p == periodoMes nuevo &&
        periodoAnio p == periodoAnio nuevo
    ) lista


-- Agregar presupuesto al estado
agregarPresupuesto :: Presupuesto -> EstadoSistema -> (EstadoSistema, Bool)
agregarPresupuesto p estado =
    if existePresupuesto p (presupuestos estado)
        then (estado, False)  -- no se agrega
        else (estado { presupuestos = p : presupuestos estado }, True)

buscarPresupuestoUnico :: String -> Int -> Int -> EstadoSistema -> Maybe Presupuesto
buscarPresupuestoUnico categoria mes anio estado =
  case filter (\p ->
        presupuestoCategoria p == categoria &&
        periodoMes p == mes &&
        periodoAnio p == anio
       ) (presupuestos estado) of

    [p] -> Just p     -- encontró exactamente uno
    []  -> Nothing    -- no encontró ninguno

mostrarPresupuesto :: Presupuesto -> String
mostrarPresupuesto p =

    "Categoría: " ++ presupuestoCategoria p ++ "\n" ++
    "Monto: " ++ show (montoMaximo p) ++ "\n" ++
    "Mes: " ++ show (periodoMes p) ++ "\n" ++
    "Año: " ++ show (periodoAnio p)
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
