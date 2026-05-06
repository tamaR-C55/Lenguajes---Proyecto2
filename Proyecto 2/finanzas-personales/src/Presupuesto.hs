module Presupuesto where
import Data.Char (toLower)


import Types 






-- =========================================================
-- MENÚ PRINCIPAL DE PRESUPUESTOS
-- =========================================================

menuPresupuestos :: EstadoSistema -> IO EstadoSistema  -- recibe un estado y lo devuelve actualizado
menuPresupuestos estado = do
    putStrLn "\n=== MENÚ DE PRESUPUESTOS ==="
    putStrLn "1. Definir presupuesto"
    putStrLn "2. Obtener informacion de un presupuesto"
    putStrLn "3. Obtener todos los presupuestos"
    putStrLn "4. Comparar Registros financieros VS Presupuesto  "
    putStrLn "0. Volver"
    putStrLn "Seleccione una opción:"
    
    opcion <- getLine --guarda la opcion 

    case opcion of
        "1" -> do
            estadoActualizado <- definirPresupuestoIO estado -- guarda el estado ya actualizado que devuelve
            menuPresupuestos estadoActualizado -- vuelve a mostrar el menu
        "2" -> do
            obtenerPresupuesto estado
            menuPresupuestos estado
        "3" -> do
            mostrarTodosPresupuestos estado
            menuPresupuestos estado
        "4" -> do
            compararPresupuesto estado
            menuPresupuestos estado
        
        "0" -> return estado -- devuelve el estado actual 
        
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

    if existePresupuesto nuevo (presupuestos estado)
        then do
            putStrLn " Ya existe un presupuesto para esa categoría y periodo"
            putStrLn "¿Desea reemplazarlo? (s/n)"
            opcion <- getLine

            if opcion == "s"
                then do
                    let estadoNuevo = reemplazarPresupuesto nuevo estado
                    putStrLn "Presupuesto reemplazado correctamente"
                    return estadoNuevo
                else do
                    putStrLn "No se realizaron cambios"
                    return estado
        else do
            let estadoNuevo = estado { presupuestos = nuevo : presupuestos estado }
            putStrLn "Presupuesto agregado correctamente"
            return estadoNuevo


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

-- calcular los gastos en un periodo determinado 
obtenerGasto :: String -> Int -> Int -> EstadoSistema -> Double
obtenerGasto categoria mes anio estado =
    let listaRegistros = registros estado
        r1 = filtrarPorTipo Gasto listaRegistros
        r2 = filtrarPorCategoria categoria r1
        r3 = filtrarPorMes mes anio r2
        total = totalMonto r3
    in total -- duelve el total 



compararPresupuesto ::  EstadoSistema -> IO()
compararPresupuesto  estado = do 
    putStrLn "\n Ingrese los datos para realizar la comparacion de Gastos VS Presupuesto"
    categoria <- leerCategoria
    mes       <- leerMes
    anio      <- leerAnio

    
    let monto =
            case buscarPresupuestoUnico categoria mes anio estado of
                Just p  -> montoMaximo p
                Nothing -> 0

    if monto ==0 
        then do
            putStrLn "No se encontró el presupuesto"
            putStrLn "Presione ENTER para volver al menú"
            _ <- getLine
            return ()
    else do
        let gasto = obtenerGasto categoria mes anio estado
        if gasto > monto
            then do
                putStrLn "\n Ha excedido su presupuesto\n"
                putStrLn ("Su presupuesto es: " ++ show monto)
                putStrLn ("Sus gastos han sido: " ++ show gasto)
                putStrLn ("Se excedió por: " ++ show (gasto - monto))


        else do
                putStrLn "No ha excedido su presupuesto\n"
                putStrLn ("Su presupuesto es: " ++ show monto)
                putStrLn ("Sus gastos han sido: " ++ show gasto)
                putStrLn ("Le queda: " ++ show (monto - gasto))




verificarAlertaPresupuesto :: String -> Int -> Int -> EstadoSistema -> IO()
verificarAlertaPresupuesto categoria mes anio estado = do 
    

    
    let monto =
            case buscarPresupuestoUnico categoria mes anio estado of
                Just p  -> montoMaximo p
                Nothing -> 0

    if monto /= 0
        then do
            let gasto = obtenerGasto categoria mes anio estado
            if gasto > monto
                then do
                    putStrLn "ALERTA "
                    putStrLn "Ha excedido su presupuesto"
                    putStrLn ("Categoria de presupuesto: " ++ show categoria)
                    putStrLn ("Su presupuesto es: " ++ show monto)
                    putStrLn ("Sus gastos han sido: " ++ show gasto)
                    putStrLn ("Se excedió por: " ++ show (gasto - monto))
                
            else
                return ()
    else do
        return ()         
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
        map toLower (presupuestoCategoria p) == map toLower (presupuestoCategoria nuevo) &&
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
        map toLower (presupuestoCategoria p) == map toLower categoria &&
        periodoMes p == mes &&
        periodoAnio p == anio
       ) (presupuestos estado) of

    [p] -> Just p     -- encontró exactamente uno
    []  -> Nothing    -- no encontró ninguno


reemplazarPresupuesto :: Presupuesto -> EstadoSistema -> EstadoSistema
reemplazarPresupuesto nuevo estado =
    estado {
        presupuestos = nuevo : filter (\p ->
            not (
                map toLower (presupuestoCategoria p) == map toLower (presupuestoCategoria nuevo) &&
                periodoMes p == periodoMes nuevo &&
                periodoAnio p == periodoAnio nuevo
            )
        ) (presupuestos estado)
    }
mostrarPresupuesto :: Presupuesto -> String
mostrarPresupuesto p =

    "Categoría: " ++ presupuestoCategoria p ++ "\n" ++
    "Monto: " ++ show (montoMaximo p) ++ "\n" ++
    "Mes: " ++ show (periodoMes p) ++ "\n" ++
    "Año: " ++ show (periodoAnio p)

mostrarTodosPresupuestos :: EstadoSistema -> IO ()
mostrarTodosPresupuestos estado = do
    let lista = presupuestos estado

    if null lista
        then putStrLn "No hay presupuestos registrados."
        else do
            putStrLn "\n--- Lista de presupuestos ---"
            putStrLn (unlines (map mostrarPresupuesto lista))
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


-- Filtra por tipo usando filter (función de orden superior)
filtrarPorTipo :: TipoRegistro -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorTipo t = filter (\r -> tipo r == t)

-- Filtra por categoría ignorando mayúsculas/minúsculas
filtrarPorCategoria :: String -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorCategoria cat =
    filter (\r -> map toLower (categoria r) == map toLower cat)

-- Filtra por mes y año
filtrarPorMes :: Int -> Int -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorMes m a = filter (\r -> mismoMesAnio (fecha r) m a)

-- Suma todos los montos usando foldr (función de orden superior)
totalMonto :: [RegistroFinanciero] -> Double
totalMonto = foldr (\r acc -> monto r + acc) 0.0