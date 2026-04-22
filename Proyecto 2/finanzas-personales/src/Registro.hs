-- Módulo de registro y gestión de registros financieros 

-- Este módulo permite al usuario registrar desde la consola:
--   Ingresos
--   Gastos
--   Ahorros
--   Inversiones
-- Cada registro incluye:
--   Monto
--   Categoría
--   Fecha
--   Descripción
--   Etiquetas múltiples (ej: "fijo", "variable")

-- También contiene funciones para listar, filtrar y calcular totales usando map, filter y foldr.

-- Depende de: Types.hs
module Registro where

-- SECCIÓN 1: IMPORTACIONES
-- ============================================================

import Types
import Data.List  (intercalate, sortBy)
import Data.Ord   (comparing)
import Data.Char  (toLower)
-- Data.Char nos da 'toLower' para convertir letras a minúsculas
-- Lo usamos para comparar texto sin importar mayúsculas/minúsculas

import System.IO (hFlush, stdout)
-- hFlush stdout fuerza que el texto aparezca en pantalla
-- antes de esperar que el usuario escriba algo.

-- SECCIÓN 2: MENÚ PRINCIPAL DE REGISTROS

-- Esta es la función que llama el Main cuando el usuario elige "Registros" en el menú principal.
-- Muestra un submenú con todas las opciones disponibles.

-- IO () significa que esta función hace entrada/salida
-- ============================================================

menuRegistros :: EstadoSistema -> IO EstadoSistema
-- Recibe el estado actual y devuelve el estado actualizado
-- después de que el usuario haga lo que quiera hacer.
menuRegistros estado = do
    -- 'do' nos permite escribir acciones de IO en secuencia,
    putStrLn ""
    putStrLn "╔══════════════════════════════════╗"
    putStrLn "║      GESTIÓN DE REGISTROS        ║"
    putStrLn "╠══════════════════════════════════╣"
    putStrLn "║  1. Registrar ingreso            ║"
    putStrLn "║  2. Registrar gasto              ║"
    putStrLn "║  3. Registrar ahorro             ║"
    putStrLn "║  4. Registrar inversión          ║"
    putStrLn "║  5. Ver todos los registros      ║"
    putStrLn "║  6. Buscar por categoría         ║"
    putStrLn "║  7. Ver registros por mes        ║"
    putStrLn "║  8. Eliminar un registro         ║"
    putStrLn "║  9. Ver resumen de totales       ║"
    putStrLn "║  0. Volver al menú principal     ║"
    putStrLn "╚══════════════════════════════════╝"
    putStr "Elige una opción: "
    hFlush stdout
    -- hFlush stdout es necesario aqui porque putStr (sin salto
    -- de linea) no siempre se muestra antes de que getLine
    -- espere la entrada del usuario en Windows.

    opcion <- getLine
    -- 'getLine' lee una línea de texto del teclado
    -- '<-' extrae el valor del IO y lo guarda en 'opcion'
    -- Después del '<-', opcion es un String normal

    case opcion of
        "1" -> do
            nuevoEstado <- pedirRegistro estado Ingreso
            menuRegistros nuevoEstado
        "2" -> do
            nuevoEstado <- pedirRegistro estado Gasto
            menuRegistros nuevoEstado
        "3" -> do
            nuevoEstado <- pedirRegistro estado Ahorro
            menuRegistros nuevoEstado
        "4" -> do
            nuevoEstado <- pedirRegistro estado Inversion
            menuRegistros nuevoEstado
        "5" -> do
            mostrarTodosLosRegistros (registros estado)
            menuRegistros estado
        "6" -> do
            nuevoEstado <- buscarPorCategoriaIO estado
            menuRegistros nuevoEstado
        "7" -> do
            mostrarPorMesIO (registros estado)
            menuRegistros estado
        "8" -> do
            nuevoEstado <- eliminarRegistroIO estado
            menuRegistros nuevoEstado
        "9" -> do
            putStrLn (resumenTotales (registros estado))
            menuRegistros estado
        "0" -> do
            putStrLn "Volviendo al menú principal..."
            return estado
            -- 'return' en Haskell NO detiene la función. Simplemente envuelve un valor
            -- en el tipo IO para poder devolverlo.
        _   -> do
            putStrLn "Opción no válida. Intenta de nuevo."
            menuRegistros estado
            -- '_' Si el usuario escribe algo que no es 0-9, llega aquí


-- SECCIÓN 3: PEDIR DATOS PARA UN NUEVO REGISTRO

-- Esta función le pregunta al usuario todos los datos necesarios para crear un registro financiero completo.

-- Usa IO para leer del teclado y valida cada campo.
-- ============================================================

pedirRegistro :: EstadoSistema -> TipoRegistro -> IO EstadoSistema
pedirRegistro estado t = do
    putStrLn ""
    putStrLn ("--- Nuevo " ++ mostrarTipo t ++ " ---")

    -- PASO 1: Pedir el monto
    m <- pedirMonto
    -- pedirMonto verifica que sea un número válido mayor a cero.

    -- PASO 2: Pedir la categoría
    putStr "Categoría (ej: Alimentación, Salario, Renta): "
    hFlush stdout
    cat <- getLine

    -- PASO 3: Pedir la fecha
    putStrLn "Fecha del registro:"
    hFlush stdout
    f <- pedirFecha
    -- pedirFecha le pide día, mes y año por separado

    -- PASO 4: Pedir la descripción
    putStr "Descripción: "
    hFlush stdout
    desc <- getLine

    -- PASO 5: Pedir las etiquetas
    putStrLn "Etiquetas separadas por coma (ej: fijo,variable,mensual)"
    putStr "Etiquetas (o Enter para ninguna): "
    hFlush stdout
    linea <- getLine
    let tags = parsearEtiquetas linea
    -- 'let' dentro de un bloque 'do' define un valor local
    -- sin necesidad de hacer IO (no lee ni imprime nada).
    -- parsearEtiquetas convierte "fijo,variable" en ["fijo","variable"]

    -- PASO 6: Validar y guardar
    case validarRegistro m cat desc of
        Left errorMsg -> do
            -- Left significa que validarRegistro encontró un error
            putStrLn ("Error: " ++ errorMsg)
            putStrLn "Intenta registrar de nuevo."
            return estado
            -- Devolvemos el estado sin cambios

        Right () -> do
            -- Right () significa que todo está bien, guardamos
            let rsActuales     = registros estado
                nuevoId        = siguienteId rsActuales
                nuevoRegistro  = RegistroFinanciero
                    { registroId  = nuevoId
                    , tipo        = t
                    , monto       = m
                    , categoria   = cat
                    , fecha       = f
                    , descripcion = desc
                    , etiquetas   = tags
                    }
                rsActualizados = rsActuales ++ [nuevoRegistro]
                -- Agregamos el nuevo registro al final de la lista.
                -- (++) concatena dos listas. [nuevoRegistro] es una
                -- lista con un solo elemento.
                nuevoEstado    = estado { registros = rsActualizados }
                -- estado { registros = ... } es "record update syntax":
                -- crea un estado nuevo igual al anterior pero con
                -- el campo 'registros' reemplazado por rsActualizados.

            putStrLn ""
            putStrLn (" " ++ mostrarTipo t ++ " registrado exitosamente.")
            putStrLn ("  ID:        " ++ show nuevoId)
            putStrLn ("  Monto:     " ++ show m)
            putStrLn ("  Categoría: " ++ cat)
            putStrLn ("  Fecha:     " ++ mostrarFecha f)
            putStrLn ("  Etiquetas: " ++ mostrarEtiquetas tags)
            return nuevoEstado


-- SECCIÓN 4: PEDIR MONTO CON VALIDACIÓN
-- Lee un número y verifica que sea válido.
-- Si el usuario escribe letras u otro texto, le pide que lo intente de nuevo (usando recursión).
-- ============================================================

pedirMonto :: IO Double
pedirMonto = do
    putStr "Monto ( ): "
    hFlush stdout

    linea <- getLine
    -- 'reads' intenta convertir un String a un tipo numérico.
    -- Devuelve [(valor, restoDelString)].
    -- Si el String no es un número, devuelve lista vacía [].
    case reads linea :: [(Double, String)] of
        [(n, "")] ->
            -- Se parseó bien y no quedó texto sobrante
            if n > 0
                then return n
                -- Número válido, lo devolvemos con return
                else do
                    putStrLn "El monto debe ser mayor a cero. Intenta de nuevo."
                    pedirMonto
                    -- Recursión: volvemos a pedir
        _ -> do
            -- El parseo falló 
            putStrLn "Monto inválido. Escribe solo números (ej: 15000.50)."
            pedirMonto


-- SECCIÓN 5: PEDIR FECHA CON VALIDACIÓN
-- Le pide al usuario día, mes y año por separado.
-- Valida que los valores estén en rangos correctos.
-- ============================================================

pedirFecha :: IO Fecha
pedirFecha = do
    d <- pedirNumeroEnRango "  Día   (1-31): " 1 31
    m <- pedirNumeroEnRango "  Mes   (1-12): " 1 12
    a <- pedirNumeroEnRango "  Año   (ej: 2025): " 2000 2100
    return (Fecha d m a)
    -- Construimos el tipo Fecha con los tres valores leídos.
    -- Fecha está definido como:
    -- data Fecha = Fecha { dia :: Int, mes :: Int, anio :: Int }


-- Función auxiliar: pide un número entero dentro de un rango
-- y vuelve a pedir si el número está fuera del rango o es inválido
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
            putStrLn "Número inválido. Intenta de nuevo."
            pedirNumeroEnRango mensaje minVal maxVal


-- SECCIÓN 6: PARSEAR ETIQUETAS

-- Convierte el texto "fijo,variable,mensual" en la lista
-- ["fijo", "variable", "mensual"].

-- Esto es procesamiento de texto puro sin IO.
-- Usamos recursión y pattern matching sobre caracteres.
-- ============================================================

parsearEtiquetas :: String -> [String]
parsearEtiquetas "" = []
-- Caso base: texto vacío → lista vacía (sin etiquetas)
parsearEtiquetas s  = map limpiar (dividirPorComa s)
  where
    -- 'limpiar' quita espacios al inicio/final y pone en minúsculas
    -- El operador (.) encadena funciones de derecha a izquierda:
    --   map toLower . quitarEspacios significa: primero aplica quitarEspacios, luego map toLower
    limpiar = map toLower . quitarEspacios

    -- Elimina espacios al inicio y al final de un String
    quitarEspacios = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    -- dropWhile (== ' ') elimina espacios del inicio
    -- reverse invierte el String para atacar también el final

    -- Divide el String cada vez que encuentra una coma
    dividirPorComa [] = [""]
    -- Lista vacía, una lista con un String vacío
    dividirPorComa (c:cs)
        | c == ','  = "" : dividirPorComa cs
        -- Si es coma: empezamos un nuevo String vacío al frente
        | otherwise = let (primera:resto) = dividirPorComa cs
                      in  (c:primera) : resto
        -- Si no es coma: agregamos el carácter al String actual


-- SECCIÓN 7: MOSTRAR REGISTROS EN PANTALLA
-- ============================================================

-- Muestra todos los registros o avisa si no hay ninguno
mostrarTodosLosRegistros :: [RegistroFinanciero] -> IO ()
mostrarTodosLosRegistros [] =
    putStrLn "\nNo hay registros guardados todavía."
mostrarTodosLosRegistros rs = do
    putStrLn ("\nTotal de registros: " ++ show (length rs))
    -- 'mapM_' es el map para acciones IO.
    -- Aplica la función a cada elemento de la lista en orden.
    -- El '_' indica que descartamos el resultado de cada acción.
    mapM_ (\r -> do
        putStrLn ""
        putStrLn (mostrarRegistro r)) rs


-- Pide una categoría y muestra los registros que coincidan
buscarPorCategoriaIO :: EstadoSistema -> IO EstadoSistema
buscarPorCategoriaIO estado = do
    putStr "\nBuscar categoría: "
    hFlush stdout
    cat <- getLine
    let encontrados = filtrarPorCategoria cat (registros estado)
    if null encontrados
        then putStrLn ("No hay registros en la categoría '" ++ cat ++ "'.")
        else do
            putStrLn ("\nRegistros en '" ++ cat ++ "': " ++
                      show (length encontrados))
            mapM_ (\r -> do
                putStrLn ""
                putStrLn (mostrarRegistro r)) encontrados
            putStrLn ("\nTotal en esta categoría: " ++
                      show (totalPorCategoria cat (registros estado)))
    return estado


-- Pide mes y año, muestra registros de ese periodo
mostrarPorMesIO :: [RegistroFinanciero] -> IO ()
mostrarPorMesIO rs = do
    putStrLn "\n--- Ver registros por mes ---"
    m <- pedirNumeroEnRango "Mes (1-12): " 1 12
    a <- pedirNumeroEnRango "Año: " 2000 2100
    let encontrados = filtrarPorMes m a rs
    if null encontrados
        then putStrLn "No hay registros para ese mes y año."
        else do
            putStrLn ("\nRegistros de " ++ show m ++ "/" ++ show a ++
                      ": " ++ show (length encontrados))
            mapM_ (\r -> do
                putStrLn ""
                putStrLn (mostrarRegistro r)) encontrados
            putStrLn "\n--- Totales del mes ---"
            putStrLn ("Ingresos:    " ++ show (totalPorTipo Ingreso   encontrados))
            putStrLn ("Gastos:      " ++ show (totalPorTipo Gasto     encontrados))
            putStrLn ("Ahorros:     " ++ show (totalPorTipo Ahorro    encontrados))
            putStrLn ("Inversiones: " ++ show (totalPorTipo Inversion encontrados))


-- Pide un ID y elimina ese registro con confirmación
eliminarRegistroIO :: EstadoSistema -> IO EstadoSistema
eliminarRegistroIO estado = do
    putStr "\nID del registro a eliminar: "
    hFlush stdout
    linea <- getLine
    case reads linea :: [(Int, String)] of
        [(idBuscar, "")] ->
            case buscarPorId (registros estado) idBuscar of
                Nothing -> do
                    putStrLn ("No existe un registro con ID " ++ show idBuscar ++ ".")
                    return estado
                Just r  -> do
                    putStrLn "\nVas a eliminar este registro:"
                    putStrLn (mostrarRegistro r)
                    putStr "¿Confirmar eliminación? (s/n): "
                    hFlush stdout
                    conf <- getLine
                    if map toLower conf == "s"
                        then do
                            let rsNuevos    = eliminarRegistro (registros estado) idBuscar
                                nuevoEstado = estado { registros = rsNuevos }
                            putStrLn "Registro eliminado correctamente."
                            return nuevoEstado
                        else do
                            putStrLn "Eliminación cancelada."
                            return estado
        _ -> do
            putStrLn "ID inválido. Debe ser un número entero."
            return estado


-- SECCIÓN 8: FUNCIONES PURAS DE DATOS

-- Estas funciones no tienen IO: solo transforman listas.
-- Las usan también los módulos de Análisis, Reportes y Reglas.
-- ============================================================

-- Genera el siguiente ID disponible
siguienteId :: [RegistroFinanciero] -> Int
siguienteId [] = 1
siguienteId rs = maximum (map registroId rs) + 1
-- map registroId rs → extrae todos los IDs como lista de Int
-- maximum → devuelve el mayor
-- +1 → el siguiente disponible

-- Elimina un registro de la lista por su ID usando filter
eliminarRegistro :: [RegistroFinanciero] -> Int -> [RegistroFinanciero]
eliminarRegistro rs idBuscar = filter (\r -> registroId r /= idBuscar) rs
-- filter se queda con todos EXCEPTO el que tiene ese ID

-- Busca un registro por ID, devuelve Maybe (puede no existir)
buscarPorId :: [RegistroFinanciero] -> Int -> Maybe RegistroFinanciero
buscarPorId [] _ = Nothing
buscarPorId (r:rs) idBuscar
    | registroId r == idBuscar = Just r
    | otherwise                = buscarPorId rs idBuscar

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

-- Filtra por etiqueta
filtrarPorEtiqueta :: String -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorEtiqueta tag = filter (\r -> tag `elem` etiquetas r)

-- Suma todos los montos usando foldr (función de orden superior)
totalMonto :: [RegistroFinanciero] -> Double
totalMonto = foldr (\r acc -> monto r + acc) 0.0
-- foldr recorre la lista de derecha a izquierda acumulando la suma

-- Total de un tipo específico
totalPorTipo :: TipoRegistro -> [RegistroFinanciero] -> Double
totalPorTipo t rs = totalMonto (filtrarPorTipo t rs)

-- Total de una categoría específica
totalPorCategoria :: String -> [RegistroFinanciero] -> Double
totalPorCategoria cat rs = totalMonto (filtrarPorCategoria cat rs)

-- Aplica descuento porcentual a todos los montos con map - para somulacion financiera
aplicarDescuento :: Double -> [RegistroFinanciero] -> [RegistroFinanciero]
aplicarDescuento pct = map (\r -> r { monto = monto r * (1 - pct / 100) })
-- map transforma cada registro creando uno nuevo con el monto reducido

-- Ordena registros de mayor a menor monto para ordenar los registros de mayor a menor monto
ordenarPorMontoDesc :: [RegistroFinanciero] -> [RegistroFinanciero]
ordenarPorMontoDesc = sortBy (flip (comparing monto))

-- Valida los datos antes de crear el registro
-- Devuelve Either: Left = error con mensaje, Right = todo bien
validarRegistro :: Double -> String -> String -> Either String ()
validarRegistro m cat desc
    | m   <= 0  = Left "El monto debe ser mayor a cero."
    | null cat  = Left "La categoría no puede estar vacía."
    | null desc = Left "La descripción no puede estar vacía."
    | otherwise = Right ()

-- Genera un resumen de totales formateado como tabla
resumenTotales :: [RegistroFinanciero] -> String
resumenTotales rs =
    "\n╔══════════════════════════════════════════╗\n"  ++
    "║           RESUMEN FINANCIERO             ║\n"  ++
    "╠══════════════════════════════════════════╣\n"  ++
    "║ Ingresos:    " ++ pad (show ingresos)   ++ "║\n" ++
    "║ Gastos:      " ++ pad (show gastos)     ++ "║\n" ++
    "║ Ahorros:     " ++ pad (show ahorros)    ++ "║\n" ++
    "║ Inversiones: " ++ pad (show inversiones)++ "║\n" ++
    "╠══════════════════════════════════════════╣\n"  ++
    "║ Balance neto: " ++ pad (show balance)    ++ "║\n" ++
    "╚══════════════════════════════════════════╝"
  where
    ingresos    = totalPorTipo Ingreso   rs
    gastos      = totalPorTipo Gasto     rs
    ahorros     = totalPorTipo Ahorro    rs
    inversiones = totalPorTipo Inversion rs
    balance     = ingresos - gastos
    -- Rellena con espacios a la derecha para alinear la tabla
    pad s = take 26 (s ++ repeat ' ')
