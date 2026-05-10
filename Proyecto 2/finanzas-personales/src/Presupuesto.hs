module Presupuesto where
import Data.Char (toLower)


import Types 






-- =========================================================
-- MENÚ PRINCIPAL DE PRESUPUESTOS
-- =========================================================

menuPresupuestos :: EstadoSistema -> IO EstadoSistema  -- recibe un estado y lo devuelve actualizado
menuPresupuestos estado = do
    putStrLn "\n=== MENU DE PRESUPUESTOS ==="
    putStrLn "1. Definir presupuesto"
    putStrLn "2. Obtener informacion de un presupuesto"
    putStrLn "3. Obtener todos los presupuestos"
    putStrLn "4. Comparar Registros financieros VS Presupuesto  "
    putStrLn "5. Modificar presupuesto  "
    putStrLn "6. Eliminar presupuesto"
    putStrLn "0. Volver"
    putStrLn "Seleccione una opcion:"
    
    opcion <- getLine --guarda la opcion 

    case opcion of
        "1" -> do
            estadoActualizado <- definirPresupuestoIO estado -- guarda el estado ya actualizado que devuelve
            menuPresupuestos estadoActualizado -- vuelve a mostrar el menu
        "2" -> do
            obtenerPresupuesto estado -- envia el estado 
            menuPresupuestos estado -- regresa al menu
        "3" -> do
            mostrarTodosPresupuestos estado
            menuPresupuestos estado
        "4" -> do
            compararPresupuesto estado
            menuPresupuestos estado


        "5" -> do
            estadoActualizado <- modificarPresupuesto estado -- guarda el estado ya actualizado que devuelve
            menuPresupuestos estadoActualizado -- vuelve a mostrar el menu

        "6" -> do
            estadoActualizado <- eliminarPresupuesto estado -- guarda el estado ya actualizado que devuelve
            menuPresupuestos estadoActualizado -- vuelve a mostrar el menu
        
        "0" -> return estado -- devuelve el estado actual 
        
        _ -> do -- cualquier otro 
            putStrLn "Opción invalida"
            menuPresupuestos estado

-----------------------------------------------------------------------------------------------------------------------
-- funcion para definir el presupuesto 

definirPresupuestoIO :: EstadoSistema -> IO EstadoSistema -- recibe estado y lo devuelve actualizado
definirPresupuestoIO estado = do
    putStrLn "\n--- Definir nuevo presupuesto ---"
    --llama a las funciones que ingresan y validan datos 
    categoria <- leerCategoria
    monto     <- leerMonto
    mes       <- leerMes
    anio      <- leerAnio

    let nuevoId = siguienteIdPresupuesto (presupuestos estado)

    let nuevo = crearPresupuesto nuevoId categoria monto mes anio--crea el nuevo presupuesto 

    if existePresupuesto nuevo (presupuestos estado) --verfica si ya existe un presupuesto con los mismo datos envia el nuevo y la lista de presupuestos
        then do
            putStrLn " Ya existe un presupuesto para esa categoria y periodo"
            putStrLn "¿Desea reemplazarlo? (s/n)"
            opcion <- getLine --lee la opcion 

            if opcion == "s"
                then do
                    let estadoNuevo = reemplazarPresupuesto nuevo estado -- llama a la funcion que remplaza con el nuevo presupuesto
                    putStrLn "Presupuesto reemplazado correctamente"
                    return estadoNuevo -- devuelve estado actualizado 
                else do
                    putStrLn "No se realizaron cambios"
                    return estado
        else do
            let estadoNuevo = estado { presupuestos = nuevo : presupuestos estado } -- actualiza el campo de registros y agrega el presupuesto a la lista
            putStrLn "Presupuesto agregado correctamente"
            return estadoNuevo


-- obtiene la informacion de un presupuesto en especifico 
obtenerPresupuesto :: EstadoSistema -> IO ()
obtenerPresupuesto estado = do 
    putStrLn "\nIngrese la informacion del presupuesto"
    -- llama a las funciones que ingresan datos y validan 
    categoria <- leerCategoria
    mes       <- leerMes
    anio      <- leerAnio

    let resultado = buscarPresupuestoUnico categoria mes anio estado -- llama a la funcion que busca el presupueto
    putStrLn  "\n"

    case resultado of
        Just p  -> putStrLn (mostrarPresupuesto p) -- si encontro un presupuesto con esos datos lo muestra
        Nothing -> putStrLn "No se encontró el presupuesto" 



-- calcular los gastos en un periodo determinado 
obtenerGasto :: String -> Int -> Int -> EstadoSistema -> Double
obtenerGasto categoria mes anio estado =
    let listaRegistros = registros estado -- obtiene la lista de registros financieros
        r1 = filtrarPorTipo Gasto listaRegistros -- obtiene los registro de tipo gasto
        r2 = filtrarPorCategoria categoria r1 -- filtra solo los de la categoria
        r3 = filtrarPorMes mes anio r2 -- trae los registros filtrados que coicida el mes y año
        total = totalMonto r3 -- se suman los montos de todos los registro del filtro
    in total -- duelve el total 


-- funcion para comparar el presupuesto con los datos reales
compararPresupuesto ::  EstadoSistema -> IO()
compararPresupuesto  estado = do 
    putStrLn "\n Ingrese los datos para realizar la comparacion de Gastos VS Presupuesto"
    categoria <- leerCategoria
    mes       <- leerMes
    anio      <- leerAnio

    
    let monto =
            case buscarPresupuestoUnico categoria mes anio estado of -- busca un presupuesto en especifico 
                Just p  -> montoMaximo p -- obtiene el monto del presupuesto que encontro
                Nothing -> 0 -- no existe ese presupuesto 

    if monto ==0  -- si el presupuesto no existe 
        then do
            putStrLn "No se encontro el presupuesto"
            putStrLn "Presione ENTER para volver al menu"
            _ <- getLine
            return ()
    else do
        let gasto = obtenerGasto categoria mes anio estado --obtiene cuanto ha castado en esa categoria en el mes y año 
        if gasto > monto-- se excedio el presupuesto 
            then do
                putStrLn "\n Ha excedido su presupuesto\n"
                putStrLn ("Su presupuesto es: " ++ show monto)
                putStrLn ("Sus gastos han sido: " ++ show gasto)
                putStrLn ("Se excedio por: " ++ show (gasto - monto))


        else do
                putStrLn "No ha excedido su presupuesto\n"
                putStrLn ("Su presupuesto es: " ++ show monto)
                putStrLn ("Sus gastos han sido: " ++ show gasto)
                putStrLn ("Le queda: " ++ show (monto - gasto))



-- funcion para generar una alerta si la persona excedio su presupuesto despues de un registro de gasto 
verificarAlertaPresupuesto :: String -> Int -> Int -> EstadoSistema -> IO()
verificarAlertaPresupuesto categoria mes anio estado = do 

    let monto =
            case buscarPresupuestoUnico categoria mes anio estado of -- busca un presupuesto en especifico 
                Just p  -> montoMaximo p -- si existe el presupuesto y obtiene el monto maximo
                Nothing -> 0

    if monto /= 0 -- si existe un presupuesto con los datos 
        then do
            let gasto = obtenerGasto categoria mes anio estado -- obtiene el total de gasto 
            if gasto > monto -- excedio el presupuesto 
                then do
                    putStrLn "ALERTA "
                    putStrLn "Ha excedido su presupuesto"
                    putStrLn ("Categoria de presupuesto: " ++ show categoria)
                    putStrLn ("Su presupuesto es: " ++ show monto)
                    putStrLn ("Sus gastos han sido: " ++ show gasto)
                    putStrLn ("Se excedio por: " ++ show (gasto - monto))
                
            else
                return () -- no lo excedio no hace nada
    else do
        return ()    -- no hay presupuesto no hace nada 

---------------------------------------------------------------------------------------------------------------------------------
-- FUNCIONES auxiliares

-- Crear presupuesto
crearPresupuesto :: Int -> String -> Double -> Int -> Int -> Presupuesto -- construye el dato
crearPresupuesto id cat monto mes anio =
    Presupuesto id cat monto mes anio-- crea el nuevo presupuesto


siguienteIdPresupuesto :: [Presupuesto] -> Int

siguienteIdPresupuesto [] = 1

siguienteIdPresupuesto ps =
    1 + maximum (map presupuestoId ps)
-- validar duplicados

existePresupuesto :: Presupuesto -> [Presupuesto] -> Bool
existePresupuesto nuevo lista =
    any (\p -> -- recorre la lista  -- devuelve true si uno cumple la condicion 
        map toLower (presupuestoCategoria p) == map toLower (presupuestoCategoria nuevo) && -- convirte a minusculas y compara las categorias 
        periodoMes p == periodoMes nuevo && -- compara el mes del nuevo con el p 
        periodoAnio p == periodoAnio nuevo -- compara el año 
    ) lista



-- Agregar presupuesto al estado
--agregarPresupuesto :: Presupuesto -> EstadoSistema -> (EstadoSistema, Bool)
--agregarPresupuesto p estado =
    --if existePresupuesto p (presupuestos estado)
      --  then (estado, False)  -- no se agrega
       -- else (estado { presupuestos = p : presupuestos estado }, True)


-- busca un presupuesto en especifico 
buscarPresupuestoUnico :: String -> Int -> Int -> EstadoSistema -> Maybe Presupuesto
buscarPresupuestoUnico categoria mes anio estado =
  case filter (\p -> -- filtra los que cumplen la condicion 
        -- para cada presupuesto p compara la categoria año y mes ignorando las mayusculas 
        map toLower (presupuestoCategoria p) == map toLower categoria &&
        periodoMes p == mes &&
        periodoAnio p == anio
       ) (presupuestos estado) of -- obtiene la lista de presupuestos

    [p] -> Just p     -- encontró exactamente uno
    []  -> Nothing    -- no encontró ninguno




reemplazarPresupuesto :: Presupuesto -> EstadoSistema -> EstadoSistema -- devuelve el estado actulizado 
reemplazarPresupuesto nuevo estado =
    estado { -- se va a actulizar la lista de prespuestos 
        presupuestos = nuevo : filter (\p -> -- recorre los presupuestos actuales  y elimina el que quiere remplazar 
            not (-- por cada elemento p ve si coincide y lo elimina
                map toLower (presupuestoCategoria p) == map toLower (presupuestoCategoria nuevo) &&
                periodoMes p == periodoMes nuevo &&
                periodoAnio p == periodoAnio nuevo
            )
        ) (presupuestos estado)
    } -- quedan todos los demas presupuestos y el que se añadio 


-- muestra la informacion de un presupuesto 
mostrarPresupuesto :: Presupuesto -> String
mostrarPresupuesto p =
    "ID: " ++ show (presupuestoId p) ++ "\n" ++
    "Categoria: " ++ presupuestoCategoria p ++ "\n" ++
    "Monto: " ++ show (montoMaximo p) ++ "\n" ++
    "Mes: " ++ show (periodoMes p) ++ "\n" ++
    "Año: " ++ show (periodoAnio p)


buscarPresupuestoPorId :: Int -> EstadoSistema -> Maybe Presupuesto

buscarPresupuestoPorId idBuscado estado =

    case filter
            (\p -> presupuestoId p == idBuscado)
            (presupuestos estado)
    of

        [p] -> Just p

        _   -> Nothing

mostrarTodosPresupuestos :: EstadoSistema -> IO ()
mostrarTodosPresupuestos estado = do
    let lista = presupuestos estado -- obtiene la lista de presupuestos 

    if null lista
        then putStrLn "No hay presupuestos registrados."
        else do
            putStrLn "\n--- Lista de presupuestos ---"
            putStrLn (unlines (map mostrarPresupuesto lista)) -- aplica la funcion de mostrar a cada elementos
            -- unlines convierte una lista de strings en uno slo con saltos


-- =========================================================
-- MODIFICAR PRESUPUESTO
-- =========================================================

modificarPresupuesto :: EstadoSistema -> IO EstadoSistema
modificarPresupuesto estado = do

    let lista = presupuestos estado

    -- =============================================
    -- VALIDAR SI HAY PRESUPUESTOS
    -- =============================================

    if null lista
        then do

            putStrLn "\nNo hay presupuestos registrados."

            return estado

        else do

            -- =====================================
            -- MOSTRAR PRESUPUESTOS
            -- =====================================

            putStrLn "\n====== PRESUPUESTOS REGISTRADOS ======\n"

            putStrLn (unlines (map mostrarPresupuesto lista))

            -- =====================================
            -- PEDIR ID
            -- =====================================

            putStrLn "\nIngrese el ID del presupuesto a modificar:"

            idStr <- getLine

            if not (esNumero idStr)
                then do

                    putStrLn "ID invalido"

                    return estado

                else do

                    let idBuscado = read idStr :: Int

                    -- =================================
                    -- BUSCAR PRESUPUESTO
                    -- =================================

                    case buscarPresupuestoPorId idBuscado estado of

                        Nothing -> do

                            putStrLn "No existe un presupuesto con ese ID."

                            return estado

                        Just presupuestoActual -> do

                            -- =========================
                            -- MOSTRAR PRESUPUESTO
                            -- =========================

                            putStrLn "\nPresupuesto encontrado:\n"

                            putStrLn
                                (mostrarPresupuesto presupuestoActual)

                            -- =========================
                            -- PEDIR NUEVO MONTO
                            -- =========================

                            

                            nuevoMonto <- leerMonto

                            -- =========================
                            -- CREAR PRESUPUESTO NUEVO
                            -- =========================

                            let presupuestoNuevo =
                                    Presupuesto
                                        { presupuestoId =
                                            presupuestoId presupuestoActual

                                        , presupuestoCategoria =
                                            presupuestoCategoria presupuestoActual

                                        , montoMaximo =
                                            nuevoMonto

                                        , periodoMes =
                                            periodoMes presupuestoActual

                                        , periodoAnio =
                                            periodoAnio presupuestoActual
                                        }

                            -- =========================
                            -- REEMPLAZAR EN LISTA
                            -- =========================

                            let nuevaLista =
                                    presupuestoNuevo :

                                    filter
                                        (\p ->
                                            presupuestoId p /= idBuscado
                                        )
                                        lista

                            let nuevoEstado =
                                    estado
                                        { presupuestos = nuevaLista }

                            putStrLn
                                "\nPresupuesto modificado correctamente."

                            return nuevoEstado


-- =========================================================
-- ELIMINAR PRESUPUESTO
-- =========================================================

eliminarPresupuesto :: EstadoSistema -> IO EstadoSistema
eliminarPresupuesto estado = do

    let lista = presupuestos estado

    -- =============================================
    -- VALIDAR SI HAY PRESUPUESTOS
    -- =============================================

    if null lista
        then do

            putStrLn "\nNo hay presupuestos registrados."

            return estado

        else do

            -- =====================================
            -- MOSTRAR PRESUPUESTOS
            -- =====================================

            putStrLn "\n====== PRESUPUESTOS REGISTRADOS ======\n"

            putStrLn (unlines (map mostrarPresupuesto lista))

            -- =====================================
            -- PEDIR ID
            -- =====================================

            putStrLn "\nIngrese el ID del presupuesto a eliminar:"

            idStr <- getLine

            if not (esNumero idStr)
                then do

                    putStrLn "ID invalido"

                    return estado

                else do

                    let idBuscado = read idStr :: Int

                    -- =================================
                    -- BUSCAR PRESUPUESTO
                    -- =================================

                    case buscarPresupuestoPorId idBuscado estado of

                        Nothing -> do

                            putStrLn
                                "No existe un presupuesto con ese ID."

                            return estado

                        Just presupuestoEncontrado -> do

                            -- =========================
                            -- MOSTRAR PRESUPUESTO
                            -- =========================

                            putStrLn "\nPresupuesto encontrado:\n"

                            putStrLn
                                (mostrarPresupuesto presupuestoEncontrado)

                            -- =========================
                            -- CONFIRMAR ELIMINACIÓN
                            -- =========================

                            putStrLn
                                "\n¿Seguro que desea eliminar este presupuesto? (s/n)"

                            opcion <- getLine

                            if opcion == "s"
                                then do

                                    -- =================
                                    -- ELIMINAR
                                    -- =================

                                    let nuevaLista =
                                            filter
                                                (\p ->
                                                    presupuestoId p /= idBuscado
                                                )
                                                lista

                                    let nuevoEstado =
                                            estado
                                                { presupuestos = nuevaLista }

                                    putStrLn
                                        "\nPresupuesto eliminado correctamente."

                                    return nuevoEstado

                                else do

                                    putStrLn
                                        "\nOperacion cancelada."

                                    return estado
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

    let categorias = categoriasPorTipo Gasto

    putStrLn "\nSeleccione una categoria:"

    mostrarCategorias 1 categorias

    opcionStr <- getLine

    if not (esNumero opcionStr)
        then do
            putStrLn "Opcion invalida"
            leerCategoria

        else do

            let opcion = read opcionStr :: Int

            if opcion < 1 || opcion > length categorias
                then do
                    putStrLn "Opcion fuera de rango"
                    leerCategoria

                else
                    return (categorias !! (opcion - 1))
mostrarCategorias :: Int -> [String] -> IO ()

mostrarCategorias _ [] = return ()

mostrarCategorias n (c:cs) = do

    putStrLn (show n ++ ". " ++ c)

    mostrarCategorias (n + 1) cs


leerMonto :: IO Double
leerMonto = do
    putStrLn "Ingrese el monto maximo:"
    montoStr <- getLine
    if not (esNumero montoStr)
        then do
            putStrLn "Monto invalido"
            leerMonto
        else return (read montoStr) -- convierte a nuemero 

leerMes :: IO Int
leerMes = do
    putStrLn "Ingrese el mes (1-12):"
    mesStr <- getLine
    if not (esNumero mesStr)
        then do
            putStrLn "Mes invalido"
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
            putStrLn "Año invalido"
            leerAnio
        else do
            let anio = read anioStr
            if anio <= 0
                then do
                    putStrLn "Año invalido"
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