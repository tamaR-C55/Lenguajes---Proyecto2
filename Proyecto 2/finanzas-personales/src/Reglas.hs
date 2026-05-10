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

            let yaExiste =
                    existeReglaCategoria
                        categoria
                        (reglas estado)

            monto <- leerMontoRegla

            putStrLn "Ingrese el mensaje de alerta:"
            mensaje <- getLine


            -- Obtener nuevo ID
            let nuevoId =
                    siguienteIdRegla (reglas estado)


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


            -- Lista actual
            let reglasActuales = reglas estado


            -- =====================================
            -- SI YA EXISTE UNA REGLA
            -- =====================================

            if yaExiste
                then do

                    putStrLn
                        "Ya existe una regla para esta categoria."

                    putStrLn
                        "¿Desea reemplazarla? (s/n)"

                    opcionReemplazo <- getLine

                    if opcionReemplazo == "s"
                        then do

                            let nuevasReglas =
                                    reemplazarReglaCategoria
                                        nuevaRegla
                                        reglasActuales

                            let estadoNuevo =
                                    estado
                                        { reglas = nuevasReglas }

                            putStrLn
                                "Regla reemplazada correctamente."

                            return estadoNuevo

                        else do

                            putStrLn
                                "No se realizaron cambios."

                            return estado


                -- =================================
                -- SI NO EXISTE
                -- =================================

                else do

                    let estadoNuevo =
                            estado
                                { reglas =
                                    nuevaRegla : reglasActuales
                                }

                    putStrLn
                        "Regla creada correctamente"

                    return estadoNuevo


                -- =========================================
        -- REGLA DE AHORRO
        -- =========================================

        "2" -> do

            -- Buscar si ya existe una regla de ahorro
            let reglaExistente =
                    filter
                        (\r ->
                            case condicion r of
                                AhorroMenorA _ -> True
                                _              -> False
                        )
                        (reglas estado)

            -- =====================================
            -- SI YA EXISTE
            -- =====================================

            if not (null reglaExistente)
                then do

                    putStrLn "\nYa existe una regla de ahorro."

                    putStrLn "Desea reemplazarla? (s/n)"

                    respuesta <- getLine

                    if respuesta == "s"
                        then do

                            monto <- leerMontoRegla

                            putStrLn "Ingrese el mensaje de advertencia:"
                            mensaje <- getLine

                            let nuevoId =
                                    siguienteIdRegla (reglas estado)

                            let condicionNueva =
                                    AhorroMenorA monto

                            let nuevaRegla =
                                    ReglaSistema
                                        { reglaId = nuevoId
                                        , condicion = condicionNueva
                                        , mensajeAlerta = mensaje
                                        }

                            -- Eliminar reglas de ahorro anteriores
                            let reglasFiltradas =
                                    filter
                                        (\r ->
                                            case condicion r of
                                                AhorroMenorA _ -> False
                                                _              -> True
                                        )
                                        (reglas estado)

                            let estadoNuevo =
                                    estado
                                        { reglas = nuevaRegla : reglasFiltradas }

                            putStrLn "Regla reemplazada correctamente"

                            return estadoNuevo

                        else do

                            putStrLn "No se realizaron cambios"

                            return estado

                -- =====================================
                -- SI NO EXISTE
                -- =====================================

                else do

                    monto <- leerMontoRegla

                    putStrLn "Ingrese el mensaje de advertencia:"
                    mensaje <- getLine

                    let nuevoId =
                            siguienteIdRegla (reglas estado)

                    let condicionNueva =
                            AhorroMenorA monto

                    let nuevaRegla =
                            ReglaSistema
                                { reglaId = nuevoId
                                , condicion = condicionNueva
                                , mensajeAlerta = mensaje
                                }

                    let estadoNuevo =
                            estado
                                { reglas = nuevaRegla : reglas estado }

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
-- VERIFICAR SI YA EXISTE UNA REGLA
-- PARA UNA CATEGORIA
-- =========================================================

existeReglaCategoria :: String -> [ReglaSistema] -> Bool

existeReglaCategoria categoriaBuscada listaReglas =

    any
        (\r ->

            case condicion r of

                GastoSuperaMonto categoria _ ->

                    categoria == categoriaBuscada

                _ ->

                    False
        )
        listaReglas


-- =========================================================
-- REEMPLAZAR REGLA DE CATEGORIA
-- =========================================================

reemplazarReglaCategoria
    :: ReglaSistema
    -> [ReglaSistema]
    -> [ReglaSistema]

reemplazarReglaCategoria nuevaRegla listaReglas =

    nuevaRegla :

    filter
        (\r ->

            case condicion r of

                GastoSuperaMonto categoria _ ->

                    case condicion nuevaRegla of

                        GastoSuperaMonto nuevaCategoria _ ->

                            categoria /= nuevaCategoria

                        _ -> True

                _ -> True
        )
        listaReglas


-- =========================================================
-- VALIDACIONES
-- =========================================================
leerCategoriaRegla :: IO String
leerCategoriaRegla = do

    putStrLn "\nSeleccione una categoria:"

    -- Mostrar categorias automaticamente
    mostrarCategorias 1 categoriasGasto

    opcionStr <- getLine

    if not (esNumero opcionStr)
        then do
            putStrLn "Opcion invalida"
            leerCategoriaRegla

        else do

            let opcion = read opcionStr :: Int

            if opcion < 1 || opcion > length categoriasGasto
                then do
                    putStrLn "Opcion invalida"
                    leerCategoriaRegla

                else do

                    return (categoriasGasto !! (opcion - 1))



-- =========================================================
-- MOSTRAR CATEGORIAS
-- =========================================================

mostrarCategorias :: Int -> [String] -> IO ()

mostrarCategorias _ [] = return ()

mostrarCategorias n (c:cs) = do

    putStrLn (show n ++ ". " ++ c)

    mostrarCategorias (n + 1) cs



leerMontoRegla :: IO Double
leerMontoRegla = do

    putStrLn "Ingrese el monto:"
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
    

-- =========================================================
-- VERIFICAR REGLAS DE GASTO
-- =========================================================

verificarReglasGasto :: String -> EstadoSistema -> IO ()
verificarReglasGasto categoriaRegistro estado = do

    let listaReglas = reglas estado

    verificarListaReglas categoriaRegistro listaReglas estado



-- =========================================================
-- RECORRER LISTA DE REGLAS
-- =========================================================

verificarListaReglas :: String -> [ReglaSistema] -> EstadoSistema -> IO ()

verificarListaReglas _ [] _ = return ()

verificarListaReglas categoriaRegistro (r:rs) estado = do

    -- Revisar condición de la regla
    case condicion r of

        -- =========================================
        -- REGLA DE GASTO
        -- =========================================

        GastoSuperaMonto categoria limite -> do

            -- SOLO revisar si la categoria coincide
            if categoria == categoriaRegistro
                then do

                    -- Obtener total gastado
                    let gastoTotal =
                            obtenerGastoTotalCategoria categoria estado

                    -- Verificar si supera límite
                    if gastoTotal > limite
                        then do

                            putStrLn "\nALERTA DE REGLA"

                            putStrLn ("Categoria: " ++ categoria)

                            putStrLn ("Limite definido: " ++ show limite)

                            putStrLn ("Gasto actual: " ++ show gastoTotal)

                            putStrLn ("Mensaje: " ++ mensajeAlerta r)

                        else
                            return ()

                else
                    return ()


        -- =========================================
        -- SI ES OTRA REGLA
        -- =========================================

        _ ->
            return ()


    -- Seguir revisando el resto
    verificarListaReglas categoriaRegistro rs estado



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

-- =========================================================
-- OBTENER AHORRO REAL
-- =========================================================

obtenerTotalAhorro :: EstadoSistema -> Double
obtenerTotalAhorro estado =

    let listaRegistros = registros estado

        -- =================================================
        -- INGRESOS EN AHORRO
        -- Dinero que entra al ahorro
        -- =================================================

        ingresosAhorro =
            filter
                (\r ->
                    tipo r == Ingreso &&
                    categoria r == "Ahorro"
                )
                listaRegistros

        totalIngresos =
            sum (map monto ingresosAhorro)


        -- =================================================
        -- GASTOS EN AHORRO
        -- Dinero retirado del ahorro
        -- =================================================

        gastosAhorro =
            filter
                (\r ->
                    tipo r == Gasto &&
                    categoria r == "Ahorro"
                )
                listaRegistros

        totalGastos =
            sum (map monto gastosAhorro)

    in

        totalIngresos - totalGastos

-- Verifica si un string es número
esNumero :: String -> Bool
esNumero s = case reads s :: [(Double, String)] of -- intenta convertir a numero
    [(n, "")] -> True -- su se pudo convertir 
    _         -> False
