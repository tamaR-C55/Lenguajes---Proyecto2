-- Modulo de registro y gestion de registros financieros 

-- Este modulo permite al usuario registrar desde la consola:
--   Ingresos
--   Gastos
--   Ahorros
--   Inversiones
-- Cada registro incluye:
--   Monto
--   Categoria
--   Fecha
--   Descripcion
--   Etiquetas multiples (ej: "fijo", "variable")

-- Tambien contiene funciones para listar, filtrar y calcular totales usando map, filter y foldr.

-- Depende de: Types.hs
module Registro where

-- SECCIoN 1: IMPORTACIONES
-- ============================================================

import Types
import Data.List  (sortBy)
import Data.Ord   (comparing)
import Data.Char  (toLower)
-- Data.Char nos da 'toLower' para convertir letras a minusculas
-- Lo usamos para comparar texto sin importar mayusculas/minusculas

import System.IO (hFlush, stdout)
import Presupuesto hiding (totalMonto, filtrarPorTipo, filtrarPorCategoria,filtrarPorMes )
import Reglas
-- hFlush stdout fuerza que el texto aparezca en pantalla
-- antes de esperar que el usuario escriba algo.

-- SECCIoN 2: MENu PRINCIPAL DE REGISTROS

-- Esta es la funcion que llama el Main cuando el usuario elige "Registros" en el menu principal.
-- Muestra un submenu con todas las opciones disponibles.

-- IO () significa que esta funcion hace entrada/salida
-- ============================================================
menuRegistros :: EstadoSistema -> IO EstadoSistema
menuRegistros estado = do
    putStrLn ""
    putStrLn "======================================"
    putStrLn "       GESTION DE REGISTROS"
    putStrLn "======================================"
    putStrLn "  1. Registrar ingreso"
    putStrLn "  2. Registrar gasto"
    putStrLn "  3. Ver todos los registros"
    putStrLn "  4. Buscar por categoria"
    putStrLn "  5. Ver registros por mes"
    putStrLn "  6. Eliminar un registro"
    putStrLn "  0. Volver al menu principal"
    putStrLn "======================================"
    putStr "Elige una opcion: "
    hFlush stdout

    opcion <- getLine

    case opcion of
        "1" -> do
            nuevoEstado <- pedirRegistro estado Ingreso
            menuRegistros nuevoEstado
        "2" -> do
            nuevoEstado <- pedirRegistro estado Gasto
            menuRegistros nuevoEstado
        "3" -> do
            mostrarTodosLosRegistros (registros estado)
            menuRegistros estado
        "4" -> do
            nuevoEstado <- buscarPorCategoriaIO estado
            menuRegistros nuevoEstado
        "5" -> do
            mostrarPorMesIO (registros estado)
            menuRegistros estado
        "6" -> do
            nuevoEstado <- eliminarRegistroIO estado
            menuRegistros nuevoEstado
        "0" -> do
            putStrLn "Volviendo al menu principal..."
            return estado
        _ -> do
            putStrLn "Opcion no valida. Intenta de nuevo."
            menuRegistros estado


-- ============================================================
-- PEDIR DATOS PARA UN NUEVO REGISTRO
-- ============================================================

pedirRegistro :: EstadoSistema -> TipoRegistro -> IO EstadoSistema
pedirRegistro estado t = do
    putStrLn ""
    putStrLn ("--- Nuevo " ++ mostrarTipo t ++ " ---")

    -- PASO 1: Categoria con opciones numeradas
    -- En vez de que el usuario escriba lo que quiera,
    -- le mostramos las opciones disponibles y elige un numero.
    cat <- pedirCategoria t

    -- PASO 2: Monto
    m <- pedirMonto

    -- PASO 3: Fecha
    putStrLn "Fecha del registro:"
    f <- pedirFecha

    -- PASO 4: Descripcion
    putStr "Descripcion: "
    hFlush stdout
    desc <- getLine

    -- PASO 5: Etiquetas
    putStrLn "Etiquetas separadas por coma (ej: fijo,variable,mensual)"
    putStr "Etiquetas (o Enter para ninguna): "
    hFlush stdout
    linea <- getLine
    let tags = parsearEtiquetas linea

    -- PASO 6: Validar y guardar
    case validarRegistro m cat desc of
        Left errorMsg -> do
            putStrLn ("Error: " ++ errorMsg)
            putStrLn "Intenta registrar de nuevo."
            return estado

        Right () -> do
            let rsActuales    = registros estado
                nuevoId       = siguienteId rsActuales
                nuevoRegistro = RegistroFinanciero
                    { registroId  = nuevoId
                    , tipo        = t
                    , monto       = m
                    , categoria   = cat
                    , fecha       = f
                    , descripcion = desc
                    , etiquetas   = tags
                    }
                nuevoEstado   = estado { registros = rsActuales ++ [nuevoRegistro] }

            putStrLn ""
            putStrLn "Registro guardado exitosamente!"
            putStrLn ("  ID:        " ++ show nuevoId)
            putStrLn ("  Tipo:      " ++ mostrarTipo t)
            putStrLn ("  Categoria: " ++ cat)
            putStrLn ("  Monto:     " ++ show m)
            putStrLn ("  Fecha:     " ++ mostrarFecha f)
            putStrLn ("  Etiquetas: " ++ mostrarEtiquetas tags)
            return nuevoEstado


-- ============================================================
-- PEDIR CATEGORIA CON OPCIONES NUMERADAS
--
-- Muestra la lista de categorias segun el tipo (Ingreso/Gasto)
-- y el usuario solo escoge el numero.
-- Esto garantiza que los nombres de categorias sean consistentes,
-- lo que es necesario para que los presupuestos funcionen bien.
-- ============================================================

pedirCategoria :: TipoRegistro -> IO String
pedirCategoria t = do
    let cats = categoriasPorTipo t
    -- categoriasPorTipo esta definida en Types.hs y devuelve
    -- la lista correcta segun si es Ingreso o Gasto
    putStrLn ("\nCategorias de " ++ mostrarTipo t ++ ":")
    -- 'zip [1..] cats' empareja cada categoria con su numero:
    -- [(1,"Salario"), (2,"Freelance"), ...]
    -- 'mapM_' aplica la funcion a cada elemento de la lista en IO
    mapM_ (\(n, c) -> putStrLn ("  " ++ show n ++ ". " ++ c))
          (zip [1..] cats)
    putStr "Elige el numero de categoria: "
    hFlush stdout
    linea <- getLine
    case reads linea :: [(Int, String)] of
        [(n, "")] ->
            if n >= 1 && n <= length cats
                then return (cats !! (n - 1))
                -- (!!) accede al elemento en la posicion dada.
                -- (n-1) porque las listas en Haskell empiezan en 0
                -- pero le mostramos al usuario desde 1.
                else do
                    putStrLn ("Elige un numero entre 1 y " ++ show (length cats) ++ ".")
                    pedirCategoria t
        _ -> do
            putStrLn "Opcion invalida. Escribe solo el numero."
            pedirCategoria t


-- ============================================================
-- PEDIR MONTO CON VALIDACION
-- ============================================================

pedirMonto :: IO Double
pedirMonto = do
    putStr "Monto: "
    hFlush stdout
    linea <- getLine
    case reads linea :: [(Double, String)] of
        [(n, "")] ->
            if n > 0
                then return n
                else do
                    putStrLn "El monto debe ser mayor a cero. Intenta de nuevo."
                    pedirMonto
        _ -> do
            putStrLn "Monto invalido. Escribe solo numeros (ej: 15000.50)."
            pedirMonto


-- ============================================================
-- PEDIR FECHA CON VALIDACION
-- ============================================================

pedirFecha :: IO Fecha
pedirFecha = do
    d <- pedirNumeroEnRango "  Dia   (1-31): " 1 31
    m <- pedirNumeroEnRango "  Mes   (1-12): " 1 12
    a <- pedirNumeroEnRango "  Anio  (ej: 2025): " 2000 2100
    return (Fecha d m a)

-- Pide un numero entero dentro de un rango, con recursion si es invalido
pedirNumeroEnRango :: String -> Int -> Int -> IO Int
pedirNumeroEnRango mensaje minVal maxVal = do
    putStr mensaje
    hFlush stdout
    linea <- getLine
    case reads linea :: [(Int, String)] of
        [(n, "")] ->
            if n >= minVal && n <= maxVal
                then return n
                else do
                    putStrLn ("Debe estar entre " ++ show minVal ++
                              " y " ++ show maxVal ++ ". Intenta de nuevo.")
                    pedirNumeroEnRango mensaje minVal maxVal
        _ -> do
            putStrLn "Numero invalido. Intenta de nuevo."
            pedirNumeroEnRango mensaje minVal maxVal


-- ============================================================
-- PARSEAR ETIQUETAS
-- Convierte "fijo,variable" en ["fijo","variable"]
-- ============================================================

parsearEtiquetas :: String -> [String]
parsearEtiquetas "" = []
parsearEtiquetas s  = map limpiar (dividirPorComa s)
  where
    limpiar        = map toLower . quitarEspacios
    quitarEspacios = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    dividirPorComa []     = [""]
    dividirPorComa (c:cs)
        | c == ','  = "" : dividirPorComa cs
        | otherwise = let (primera:resto) = dividirPorComa cs
                      in  (c:primera) : resto


-- ============================================================
-- MOSTRAR TODOS LOS REGISTROS
--
-- Muestra el ID de cada registro claramente para que el
-- usuario sepa que IDs existen (util antes de eliminar).
-- ============================================================

mostrarTodosLosRegistros :: [RegistroFinanciero] -> IO ()
mostrarTodosLosRegistros [] =
    putStrLn "\nNo hay registros guardados todavia."
mostrarTodosLosRegistros rs = do
    putStrLn ("\nTotal de registros: " ++ show (length rs))
    mapM_ (\r -> do
        putStrLn ""
        putStrLn (mostrarRegistro r)) rs


-- ============================================================
-- BUSCAR POR CATEGORIA
--
-- Muestra las categorias que ya tienen registros guardados
-- para que el usuario elija en vez de escribir a ciegas.
-- ============================================================

buscarPorCategoriaIO :: EstadoSistema -> IO EstadoSistema
buscarPorCategoriaIO estado = do
    let rs = registros estado

    if null rs
        then do
            putStrLn "\nNo hay registros guardados todavia."
            return estado
        else do
            -- Obtener categorias unicas que ya tienen registros
            let cats = categoriasUnicas rs
            -- categoriasUnicas usa foldr para construir la lista
            -- sin duplicados (definida mas abajo)

            putStrLn "\nCategorias con registros:"
            mapM_ (\(n, c) -> putStrLn ("  " ++ show n ++ ". " ++ c))
                  (zip [1..] cats)
            putStr "Elige el numero de categoria (0 para cancelar): "
            hFlush stdout
            linea <- getLine

            case reads linea :: [(Int, String)] of
                [(0, "")] -> do
                    putStrLn "Busqueda cancelada."
                    return estado
                [(n, "")] ->
                    if n >= 1 && n <= length cats
                        then do
                            let catElegida  = cats !! (n - 1)
                                encontrados = filtrarPorCategoria catElegida rs
                            putStrLn ("\nRegistros en '" ++ catElegida ++ "': " ++
                                      show (length encontrados))
                            mapM_ (\r -> do
                                putStrLn ""
                                putStrLn (mostrarRegistro r)) encontrados
                            putStrLn ("\nTotal en esta categoria: " ++
                                      show (totalPorCategoria catElegida rs))
                            return estado
                        else do
                            putStrLn "Numero invalido."
                            return estado
                _ -> do
                    putStrLn "Opcion invalida."
                    return estado


-- ============================================================
-- VER REGISTROS POR MES
-- ============================================================

mostrarPorMesIO :: [RegistroFinanciero] -> IO ()
mostrarPorMesIO rs = do
    putStrLn "\n--- Ver registros por mes ---"
    m <- pedirNumeroEnRango "Mes (1-12): " 1 12
    a <- pedirNumeroEnRango "Anio: " 2000 2100
    let encontrados = filtrarPorMes m a rs
    if null encontrados
        then putStrLn "No hay registros para ese mes y anio."
        else do
            putStrLn ("\nRegistros de " ++ show m ++ "/" ++ show a ++
                      ": " ++ show (length encontrados))
            mapM_ (\r -> do
                putStrLn ""
                putStrLn (mostrarRegistro r)) encontrados
            putStrLn "\n--- Totales del mes ---"
            putStrLn ("Ingresos: " ++ show (totalPorTipo Ingreso encontrados))
            putStrLn ("Gastos:   " ++ show (totalPorTipo Gasto   encontrados))


-- ============================================================
-- ELIMINAR REGISTRO
--
-- Primero muestra todos los registros con sus IDs para que
-- el usuario pueda ver cual quiere eliminar, luego pide el ID.
-- ============================================================

eliminarRegistroIO :: EstadoSistema -> IO EstadoSistema
eliminarRegistroIO estado = do
    let rs = registros estado

    if null rs
        then do
            putStrLn "\nNo hay registros para eliminar."
            return estado
        else do
            -- Mostrar todos los registros con sus IDs primero
            -- para que el usuario no tenga que adivinar
            putStrLn "\n--- Registros existentes ---"
            -- Mostramos un resumen compacto con ID, tipo y categoria
            mapM_ (\r -> putStrLn (
                "  ID " ++ show (registroId r) ++
                " | " ++ mostrarTipo (tipo r) ++
                " | " ++ categoria r ++
                " | " ++ show (monto r) ++
                " | " ++ mostrarFecha (fecha r)
                )) rs

            putStrLn ""
            putStr "ID del registro a eliminar (0 para cancelar): "
            hFlush stdout
            linea <- getLine

            case reads linea :: [(Int, String)] of
                [(0, "")] -> do
                    putStrLn "Eliminacion cancelada."
                    return estado
                [(idBuscar, "")] ->
                    case buscarPorId rs idBuscar of
                        Nothing -> do
                            putStrLn ("No existe un registro con ID " ++
                                      show idBuscar ++ ".")
                            return estado
                        Just r -> do
                            -- Mostrar el registro completo antes de confirmar
                            putStrLn "\nVas a eliminar este registro:"
                            putStrLn (mostrarRegistro r)
                            putStr "Confirmar eliminacion (s/n): "
                            hFlush stdout
                            conf <- getLine
                            if map toLower conf == "s"
                                then do
                                    let rsNuevos    = eliminarRegistro rs idBuscar
                                        nuevoEstado = estado { registros = rsNuevos }
                                    putStrLn "Registro eliminado correctamente."
                                    return nuevoEstado
                                else do
                                    putStrLn "Eliminacion cancelada."
                                    return estado
                _ -> do
                    putStrLn "ID invalido. Debe ser un numero entero."
                    return estado


-- ============================================================
-- FUNCIONES PURAS DE DATOS
-- ============================================================

-- Genera el siguiente ID disponible
siguienteId :: [RegistroFinanciero] -> Int
siguienteId [] = 1
siguienteId rs = maximum (map registroId rs) + 1

-- Elimina un registro por ID usando filter
eliminarRegistro :: [RegistroFinanciero] -> Int -> [RegistroFinanciero]
eliminarRegistro rs idBuscar = filter (\r -> registroId r /= idBuscar) rs

-- Busca un registro por ID, devuelve Maybe
buscarPorId :: [RegistroFinanciero] -> Int -> Maybe RegistroFinanciero
buscarPorId [] _ = Nothing
buscarPorId (r:rs) idBuscar
    | registroId r == idBuscar = Just r
    | otherwise                = buscarPorId rs idBuscar

-- Filtra por tipo (Ingreso o Gasto)
filtrarPorTipo :: TipoRegistro -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorTipo t = filter (\r -> tipo r == t)

-- Filtra por categoria ignorando mayusculas
filtrarPorCategoria :: String -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorCategoria cat =
    filter (\r -> map toLower (categoria r) == map toLower cat)

-- Filtra por mes y anio
filtrarPorMes :: Int -> Int -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorMes m a = filter (\r -> mismoMesAnio (fecha r) m a)

-- Filtra por etiqueta
filtrarPorEtiqueta :: String -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorEtiqueta tag = filter (\r -> tag `elem` etiquetas r)

-- Suma montos con foldr
totalMonto :: [RegistroFinanciero] -> Double
totalMonto = foldr (\r acc -> monto r + acc) 0.0

-- Total por tipo
totalPorTipo :: TipoRegistro -> [RegistroFinanciero] -> Double
totalPorTipo t rs = totalMonto (filtrarPorTipo t rs)

-- Total por categoria
totalPorCategoria :: String -> [RegistroFinanciero] -> Double
totalPorCategoria cat rs = totalMonto (filtrarPorCategoria cat rs)

-- Extrae montos con map
obtenerMontos :: [RegistroFinanciero] -> [Double]
obtenerMontos = map monto

-- Aplica descuento porcentual con map (lo usa Persona 3)
aplicarDescuento :: Double -> [RegistroFinanciero] -> [RegistroFinanciero]
aplicarDescuento pct = map (\r -> r { monto = monto r * (1 - pct / 100) })

-- Ordena por monto descendente
ordenarPorMontoDesc :: [RegistroFinanciero] -> [RegistroFinanciero]
ordenarPorMontoDesc = sortBy (flip (comparing monto))

-- Valida los datos antes de crear el registro
validarRegistro :: Double -> String -> String -> Either String ()
validarRegistro m cat desc
    | m   <= 0  = Left "El monto debe ser mayor a cero."
    | null cat  = Left "La categoria no puede estar vacia."
    | otherwise = Right ()

-- Obtiene lista de categorias unicas que ya tienen registros
-- Usa foldr para construir la lista sin duplicados
categoriasUnicas :: [RegistroFinanciero] -> [String]
categoriasUnicas rs =
    foldr (\r acc ->
        let cat = categoria r
        in if cat `elem` acc then acc else acc ++ [cat]
    ) [] rs
-- foldr recorre la lista de registros.
-- Para cada registro revisa si su categoria ya esta en el acumulador.
-- Si ya esta (elem) la ignora. Si no esta, la agrega al final.
-- Resultado: lista de categorias sin repeticiones.

-- Resumen de totales como texto
resumenTotales :: [RegistroFinanciero] -> String
resumenTotales rs =
    "\n======================================\n" ++
    "        RESUMEN FINANCIERO\n"               ++
    "======================================\n"   ++
    "Ingresos: " ++ show (totalPorTipo Ingreso rs) ++ "\n" ++
    "Gastos:   " ++ show (totalPorTipo Gasto   rs) ++ "\n" ++
    "--------------------------------------\n"   ++
    "Balance:  " ++ show balance                   ++ "\n" ++
    "======================================"
  where
    balance = totalPorTipo Ingreso rs - totalPorTipo Gasto rs
