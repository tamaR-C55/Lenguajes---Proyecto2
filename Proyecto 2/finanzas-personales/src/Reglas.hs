module Reglas where

import Types

-- MENÚ DE REGLAS
-- =========================================================

menuReglas :: EstadoSistema -> IO EstadoSistema
menuReglas estado = do

    putStrLn "\n========== MENU DE REGLAS =========="
    putStrLn "1. Crear regla"
    putStrLn "2. Modificar regla"
    putStrLn "3. Eliminar regla"
    putStrLn "4. Consultar reglas"
    putStrLn "0. Volver"
    putStrLn "Seleccione una opcion:"

    opcion <- getLine

    case opcion of

        "1" -> do
            nuevoEstado <- crearReglaIO estado
            menuReglas nuevoEstado

        "2" -> do
           
            nuevoEstado <- modificarReglaIO estado
            menuReglas nuevoEstado
            

        "3" -> do
            nuevoEstado <- eliminarReglaIO estado
            menuReglas nuevoEstado

        "4" -> do
            mostrarReglas estado
            menuReglas estado

        "0" ->
            return estado

        _ -> do
            putStrLn "Opcion invalida"
            menuReglas estado



-- =========================================================
-- CREAR REGLA
-- =========================================================

crearReglaIO :: EstadoSistema -> IO EstadoSistema
crearReglaIO estado = do

    putStrLn "\n====== CREAR NUEVA REGLA ======"

    putStrLn "Seleccione el tipo de regla:"
    putStrLn "1. Gastos por categoria"
    putStrLn "2. Ahorro minimo"

    opcion <- getLine

    case opcion of

        -- =========================================
        -- REGLA DE GASTO
        -- =========================================

        "1" -> do

            categoria <- leerCategoriaRegla

            monto <- leerMontoRegla

            putStrLn "Ingrese el mensaje de alerta:"
            mensaje <- getLine


            -- Obtener nuevo ID
            let nuevoId = siguienteIdRegla (reglas estado)


            -- Crear condición
            let condicionNueva =
                    GastoSuperaMonto categoria monto


            -- Crear regla completa
            let nuevaRegla =
                    ReglaSistema
                        { reglaId = nuevoId
                        , condicion = condicionNueva
                        , mensajeAlerta = mensaje
                        }


            -- Agregar al estado
            let reglasActuales = reglas estado

            let estadoNuevo =
                    estado
                        { reglas = nuevaRegla : reglasActuales }


            putStrLn "Regla creada correctamente"

            return estadoNuevo


        -- =========================================
        -- REGLA DE AHORRO
        -- =========================================

        "2" -> do

            monto <- leerMontoRegla

            putStrLn "Ingrese el mensaje de advertencia:"
            mensaje <- getLine


            -- Obtener nuevo ID
            let nuevoId = siguienteIdRegla (reglas estado)


            -- Crear condición
            let condicionNueva =
                    AhorroMenorA monto


            -- Crear regla
            let nuevaRegla =
                    ReglaSistema
                        { reglaId = nuevoId
                        , condicion = condicionNueva
                        , mensajeAlerta = mensaje
                        }


            -- Actualizar estado
            let reglasActuales = reglas estado

            let estadoNuevo =
                    estado
                        { reglas = nuevaRegla : reglasActuales }


            putStrLn "Regla creada correctamente"

            return estadoNuevo


        _ -> do
            putStrLn "Opcion invalida"
            return estado



-- =========================================================
-- MOSTRAR TODAS LAS REGLAS
-- =========================================================

mostrarReglas :: EstadoSistema -> IO ()
mostrarReglas estado = do

    let lista = reglas estado

    if null lista
        then putStrLn "No hay reglas registradas"
        else do
            putStrLn "\n====== LISTA DE REGLAS ======"

            mapM_ (putStrLn . mostrarRegla) lista



-- =========================================================
-- MOSTRAR UNA REGLA
-- =========================================================

mostrarRegla :: ReglaSistema -> String
mostrarRegla r =

    case condicion r of

        GastoSuperaMonto categoria monto ->

            "ID: " ++ show (reglaId r) ++ "\n" ++
            "Tipo: Gasto\n" ++
            "Categoria: " ++ categoria ++ "\n" ++
            "Limite: " ++ show monto ++ "\n" ++
            "Mensaje: " ++ mensajeAlerta r ++ "\n"


        AhorroMenorA monto ->

            "ID: " ++ show (reglaId r) ++ "\n" ++
            "Tipo: Ahorro\n" ++
            "Monto minimo: " ++ show monto ++ "\n" ++
            "Mensaje: " ++ mensajeAlerta r ++ "\n"



-- =========================================================
-- GENERAR SIGUIENTE ID
-- =========================================================

siguienteIdRegla :: [ReglaSistema] -> Int
siguienteIdRegla [] = 1

siguienteIdRegla rs =
    maximum (map reglaId rs) + 1



-- =========================================================
-- VALIDACIONES
-- =========================================================

leerCategoriaRegla :: IO String
leerCategoriaRegla = do

    putStrLn "Seleccione una categoria:"
    putStrLn "1. Comida"
    putStrLn "2. Transporte"
    putStrLn "3. Salud"
    putStrLn "4. Entretenimiento"
    putStrLn "5. Vivienda"

    opcion <- getLine

    case opcion of

        "1" -> return "Comida"

        "2" -> return "Transporte"

        "3" -> return "Salud"

        "4" -> return "Entretenimiento"

        "5" -> return "Vivienda"

        _ -> do
            putStrLn "Categoria invalida"
            leerCategoriaRegla



leerMontoRegla :: IO Double
leerMontoRegla = do

    putStrLn "Ingrese el monto limite:"
    montoStr <- getLine

    if esNumero montoStr
        then return (read montoStr)
        else do
            putStrLn "Monto inválido"
            leerMontoRegla


-- =========================================================
-- MODIFICAR REGLA
-- =========================================================

modificarReglaIO :: EstadoSistema -> IO EstadoSistema
modificarReglaIO estado = do

    putStrLn "\n====== MODIFICAR REGLA ======"

    -- Mostrar reglas actuales
    mostrarReglas estado

    -- Pedir ID
    putStrLn "\nIngrese el ID de la regla a modificar:"
    idStr <- getLine

    if not (esNumero idStr)
        then do
            putStrLn "ID invalido"
            return estado

        else do

            let idBuscado = read idStr :: Int

            -- Buscar regla
            case buscarReglaPorId idBuscado (reglas estado) of

                Nothing -> do
                    putStrLn "No existe una regla con ese ID"
                    return estado

                Just reglaVieja -> do

                    putStrLn "\nIngrese los nuevos datos"

                    -- Revisar tipo de regla
                    case condicion reglaVieja of

                        -- =====================================
                        -- MODIFICAR REGLA DE GASTO
                        -- =====================================

                        GastoSuperaMonto _ _ -> do

                            categoria <- leerCategoriaRegla

                            monto <- leerMontoRegla

                            putStrLn "Ingrese el nuevo mensaje:"
                            mensaje <- getLine

                            -- Crear nueva condición
                            let nuevaCondicion =
                                    GastoSuperaMonto categoria monto

                            -- Crear nueva regla con MISMO ID
                            let reglaNueva =
                                    ReglaSistema
                                        { reglaId = idBuscado
                                        , condicion = nuevaCondicion
                                        , mensajeAlerta = mensaje
                                        }

                            -- Reemplazar en lista
                            let reglasActualizadas =
                                    reemplazarRegla reglaNueva (reglas estado)

                            -- Crear nuevo estado
                            let estadoNuevo =
                                    estado
                                        { reglas = reglasActualizadas }

                            putStrLn "Regla modificada correctamente"

                            return estadoNuevo



                        -- =====================================
                        -- MODIFICAR REGLA DE AHORRO
                        -- =====================================

                        AhorroMenorA _ -> do

                            monto <- leerMontoRegla

                            putStrLn "Ingrese el nuevo mensaje:"
                            mensaje <- getLine

                            -- Nueva condición
                            let nuevaCondicion =
                                    AhorroMenorA monto

                            -- Nueva regla
                            let reglaNueva =
                                    ReglaSistema
                                        { reglaId = idBuscado
                                        , condicion = nuevaCondicion
                                        , mensajeAlerta = mensaje
                                        }

                            -- Reemplazar lista
                            let reglasActualizadas =
                                    reemplazarRegla reglaNueva (reglas estado)

                            -- Nuevo estado
                            let estadoNuevo =
                                    estado
                                        { reglas = reglasActualizadas }

                            putStrLn "Regla modificada correctamente"

                            return estadoNuevo



-- =========================================================
-- BUSCAR REGLA POR ID
-- =========================================================

buscarReglaPorId :: Int -> [ReglaSistema] -> Maybe ReglaSistema
buscarReglaPorId _ [] = Nothing

buscarReglaPorId idBuscado (r:rs)

    | reglaId r == idBuscado = Just r

    | otherwise =
        buscarReglaPorId idBuscado rs



-- =========================================================
-- REEMPLAZAR REGLA
-- =========================================================

reemplazarRegla :: ReglaSistema -> [ReglaSistema] -> [ReglaSistema]
reemplazarRegla nuevaRegla lista =

    map reemplazar lista

    where

        reemplazar r

            | reglaId r == reglaId nuevaRegla = nuevaRegla

            | otherwise = r


-- =========================================================
-- ELIMINAR REGLA
-- =========================================================

eliminarReglaIO :: EstadoSistema -> IO EstadoSistema
eliminarReglaIO estado = do

    putStrLn "\n====== ELIMINAR REGLA ======"

    -- Mostrar reglas existentes
    mostrarReglas estado

    -- Pedir ID
    putStrLn "\nIngrese el ID de la regla a eliminar:"
    idStr <- getLine

    -- Validar ID
    if not (esNumero idStr)
        then do
            putStrLn "ID invalido"
            return estado

        else do

            let idBuscado = read idStr :: Int

            -- Buscar regla
            case buscarReglaPorId idBuscado (reglas estado) of

                Nothing -> do
                    putStrLn "No existe una regla con ese ID"
                    return estado

                Just reglaEncontrada -> do

                    -- Mostrar regla encontrada
                    putStrLn "\nRegla encontrada:\n"
                    putStrLn (mostrarRegla reglaEncontrada)

                    -- Confirmación
                    putStrLn "¿Esta seguro de eliminar esta regla?"
                    putStrLn "1. Sí"
                    putStrLn "2. No"

                    confirmacion <- getLine

                    case confirmacion of

                        -- ==============================
                        -- ELIMINAR
                        -- ==============================

                        "1" -> do

                            let reglasActualizadas =
                                    eliminarRegla idBuscado (reglas estado)

                            let estadoNuevo =
                                    estado
                                        { reglas = reglasActualizadas }

                            putStrLn "Regla eliminada correctamente"

                            return estadoNuevo


                        -- ==============================
                        -- CANCELAR
                        -- ==============================

                        "2" -> do
                            putStrLn "Eliminacion cancelada"
                            return estado


                        -- ==============================
                        -- OPCIÓN INVÁLIDA
                        -- ==============================

                        _ -> do
                            putStrLn "Opcion invalida"
                            return estado


-- =========================================================
-- ELIMINAR REGLA DE LA LISTA
-- =========================================================

eliminarRegla :: Int -> [ReglaSistema] -> [ReglaSistema]
eliminarRegla idBuscado lista =

    filter (\r -> reglaId r /= idBuscado) lista
    

verificarReglasGasto :: EstadoSistema -> IO ()
verificarReglasGasto estado = do

    let listaReglas = reglas estado

    verificarListaReglas listaReglas estado



-- =========================================================
-- RECORRER LISTA DE REGLAS
-- =========================================================

verificarListaReglas :: [ReglaSistema] -> EstadoSistema -> IO ()
verificarListaReglas [] _ = return ()

verificarListaReglas (r:rs) estado = do

    -- Revisar condición de la regla
    case condicion r of

        -- =========================================
        -- REGLA DE GASTO
        -- =========================================

        GastoSuperaMonto categoria limite -> do

            -- Obtener total gastado
            let gastoTotal =
                    obtenerGastoTotalCategoria categoria estado

            -- Verificar si supera límite
            if gastoTotal > limite
                then do

                    putStrLn "\n ALERTA DE REGLA "

                    putStrLn ("Categoria: " ++ categoria)

                    putStrLn ("Limite definido: " ++ show limite)

                    putStrLn ("Gasto actual: " ++ show gastoTotal)

                    putStrLn ("Mensaje: " ++ mensajeAlerta r)

                else
                    return ()


        -- =========================================
        -- SI ES OTRA REGLA
        -- =========================================

        _ ->
            return ()


    -- Seguir revisando el resto
    verificarListaReglas rs estado



-- =========================================================
-- OBTENER TOTAL DE GASTOS DE UNA CATEGORÍA
-- =========================================================

obtenerGastoTotalCategoria :: String -> EstadoSistema -> Double
obtenerGastoTotalCategoria categoriaBuscada estado =

    let listaRegistros = registros estado

        -- Solo gastos
        gastos =
            filter (\r -> tipo r == Gasto) listaRegistros

        -- Filtrar categoría
        categoriaFiltrada =
            filter
                (\r ->
                    categoria r == categoriaBuscada
                )
                gastos

    in

        sum (map monto categoriaFiltrada)



-- =========================================================
-- VERIFICAR REGLAS DE AHORRO
-- =========================================================

verificarReglasAhorro :: EstadoSistema -> IO ()
verificarReglasAhorro estado = do

    let listaReglas = reglas estado

    verificarListaReglasAhorro listaReglas estado



-- =========================================================
-- RECORRER LISTA DE REGLAS
-- =========================================================

verificarListaReglasAhorro :: [ReglaSistema] -> EstadoSistema -> IO ()
verificarListaReglasAhorro [] _ = return ()

verificarListaReglasAhorro (r:rs) estado = do

    -- Revisar condición
    case condicion r of

        -- =========================================
        -- REGLA DE AHORRO
        -- =========================================

        AhorroMenorA limite -> do

            -- Obtener ahorro total
            let ahorroTotal =
                    obtenerTotalAhorro estado

            -- Verificar condición
            if ahorroTotal < limite
                then do

                    putStrLn "\nADVERTENCIA DE AHORRO"

                    putStrLn ("Regla ID: " ++ show (reglaId r))

                    putStrLn ("Ahorro minimo esperado: " ++ show limite)

                    putStrLn ("Ahorro actual: " ++ show ahorroTotal)

                    putStrLn ("Mensaje: " ++ mensajeAlerta r)

                else
                    return ()


        -- =========================================
        -- SI ES OTRO TIPO DE REGLA
        -- =========================================

        _ ->
            return ()


    -- Revisar siguiente regla
    verificarListaReglasAhorro rs estado



-- =========================================================
-- OBTENER TOTAL DE AHORROS
-- =========================================================

obtenerTotalAhorro :: EstadoSistema -> Double
obtenerTotalAhorro estado =

    let listaRegistros = registros estado

        -- Solo registros de ahorro
        ahorros =
            --filter (\r -> tipo r == Ahorro) listaRegistros
            filter (\r -> categoria r == "Ahorro") listaRegistros

    in

        sum (map monto ahorros)

-- Verifica si un string es número
esNumero :: String -> Bool
esNumero s = case reads s :: [(Double, String)] of -- intenta convertir a numero
    [(n, "")] -> True -- su se pudo convertir 
    _         -> False
