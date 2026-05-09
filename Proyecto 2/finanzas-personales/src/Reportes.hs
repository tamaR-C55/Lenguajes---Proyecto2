module Reportes where

import Types
import Registro (filtrarPorMes, filtrarPorTipo, filtrarPorCategoria, totalMonto)
import Data.List (sortBy, nub, intercalate, groupBy)
import Data.Ord (comparing, Down(..))
import Data.Char (toLower)
import System.IO (hFlush, stdout)

-- Menu principal

menuReportes :: EstadoSistema -> IO EstadoSistema
menuReportes estado = do
    putStrLn ""
    putStrLn "╔══════════════════════════════════════╗"
    putStrLn "║          MENU DE REPORTES            ║"
    putStrLn "╠══════════════════════════════════════╣"
    putStrLn "║  1. Resumen mensual                  ║"
    putStrLn "║  2. Comparacion entre periodos       ║"
    putStrLn "║  3. Categorias con mayor gasto       ║"
    putStrLn "║  0. Volver al menu principal         ║"
    putStrLn "╚══════════════════════════════════════╝"
    putStr "Elige una opcion: "
    hFlush stdout

    opcion <- getLine

    case opcion of
        "1" -> do
            generarResumenMensualIO estado
            menuReportes estado
        "2" -> do
            generarComparacionPeriodosIO estado
            menuReportes estado
        "3" -> do
            generarCategoriasMayorGastoIO estado
            menuReportes estado
        "0" -> do
            putStrLn "Volviendo al menu principal."
            return estado
        _   -> do
            putStrLn "Opcion no valida. Intenta de nuevo."
            menuReportes estado



-- GENERAR RESUMEN MENSUAL
-- Pide el mes y el año para filtrar
generarResumenMensualIO :: EstadoSistema -> IO ()
generarResumenMensualIO estado = do
    putStrLn ""
    putStrLn "--- RESUMEN MENSUAL ---"
    m <- pedirNumeroEnRango "Mes (1-12): " 1 12
    a <- pedirNumeroEnRango "Año (ej: 2025): " 2000 2100
    
    let registrosMes = filtrarPorMes m a (registros estado)
    
    if null registrosMes
        then putStrLn ("\nNo hay registros para " ++ show m ++ "/" ++ show a)
        else putStrLn (generarResumenMensual m a registrosMes)

-- Genera el reporte de resumen mensual
generarResumenMensual :: Int -> Int -> [RegistroFinanciero] -> String
generarResumenMensual mes anio registrosMes =
    let ingresos    = totalPorTipo' Ingreso registrosMes
        gastos      = totalPorTipo' Gasto registrosMes
        --ahorros     = totalPorTipo' Ahorro registrosMes
        --inversiones = totalPorTipo' Inversion registrosMes
        balance     = ingresos - gastos
        nombreMes   = nombreDelMes mes
    in
        "\n╔════════════════════════════════════════════════╗\n" ++
        "║      RESUMEN MENSUAL: " ++ nombreMes ++ " " ++ show anio ++ "      ║\n" ++
        "╠════════════════════════════════════════════════╣\n" ++
        "║ Ingresos:        " ++ pad20 (formatoMoneda ingresos) ++ "║\n" ++
        "║ Gastos:          " ++ pad20 (formatoMoneda gastos) ++ "║\n" ++
        --"║ Ahorros:         " ++ pad20 (formatoMoneda ahorros) ++ "║\n" ++
        --"║ Inversiones:     " ++ pad20 (formatoMoneda inversiones) ++ "║\n" ++
        "╠════════════════════════════════════════════════╣\n" ++
        "║ Balance neto:    " ++ pad20 (formatoMoneda balance) ++ "║\n" ++
        "╚════════════════════════════════════════════════╝"



-- COMPARAR PERIODOS
-- Funcion para pedir los dos periodos y mostrar la la info
generarComparacionPeriodosIO :: EstadoSistema -> IO ()
generarComparacionPeriodosIO estado = do
    putStrLn ""
    putStrLn "--- COMPARACIÓN ENTRE PERIODOS ---"
    
    putStrLn "\nPrimer periodo:"
    m1 <- pedirNumeroEnRango "  Mes (1-12): " 1 12
    a1 <- pedirNumeroEnRango "  Año: " 2000 2100
    
    putStrLn "\nSegundo periodo:"
    m2 <- pedirNumeroEnRango "  Mes (1-12): " 1 12
    a2 <- pedirNumeroEnRango "  Año: " 2000 2100
    
    let registros1 = filtrarPorMes m1 a1 (registros estado)
        registros2 = filtrarPorMes m2 a2 (registros estado)
    
    if null registros1 || null registros2
        then putStrLn "\nNo hay registros para uno o ambos periodos."
        else putStrLn (generarComparacionPeriodos m1 a1 m2 a2 registros1 registros2)

-- Genera el reporte de comparacionn
generarComparacionPeriodos :: Int -> Int -> Int -> Int 
                            -> [RegistroFinanciero] -> [RegistroFinanciero] -> String
generarComparacionPeriodos m1 a1 m2 a2 regs1 regs2 =
    let gastos1 = totalPorTipo' Gasto regs1
        gastos2 = totalPorTipo' Gasto regs2
        ingresos1 = totalPorTipo' Ingreso regs1
        ingresos2 = totalPorTipo' Ingreso regs2
        
        variacionGastos = gastos2 - gastos1
        variacionIngresos = ingresos2 - ingresos1
        pctCambioGastos = if gastos1 > 0 then (variacionGastos / gastos1) * 100 else 0
        pctCambioIngresos = if ingresos1 > 0 then (variacionIngresos / ingresos1) * 100 else 0
        
        periodo1 = nombreDelMes m1 ++ " " ++ show a1
        periodo2 = nombreDelMes m2 ++ " " ++ show a2
        
        signoGastos = if variacionGastos > 0 then "↑" else if variacionGastos < 0 then "↓" else "="
        signoIngresos = if variacionIngresos > 0 then "↑" else if variacionIngresos < 0 then "↓" else "="
    in
        "\n╔════════════════════════════════════════════════════════════════╗\n" ++
        "║        COMPARACION: " ++ periodo1 ++ " vs " ++ periodo2 ++ "        ║\n" ++
        "╠════════════════════════════════════════════════════════════════╣\n" ++
        "║ GASTOS:                                                        ║\n" ++
        "║   " ++ periodo1 ++ ":     " ++ pad25 (formatoMoneda gastos1) ++ "║\n" ++
        "║   " ++ periodo2 ++ ":     " ++ pad25 (formatoMoneda gastos2) ++ "║\n" ++
        "║   Variacion: " ++ signoGastos ++ " " ++ pad30 (formatoMoneda variacionGastos ++ " (" ++ show (round pctCambioGastos) ++ "%)") ++ "║\n" ++
        "╠════════════════════════════════════════════════════════════════╣\n" ++
        "║ INGRESOS:                                                      ║\n" ++
        "║   " ++ periodo1 ++ ":     " ++ pad25 (formatoMoneda ingresos1) ++ "║\n" ++
        "║   " ++ periodo2 ++ ":     " ++ pad25 (formatoMoneda ingresos2) ++ "║\n" ++
        "║   Variacion: " ++ signoIngresos ++ " " ++ pad30 (formatoMoneda variacionIngresos ++ " (" ++ show (round pctCambioIngresos) ++ "%)") ++ "║\n" ++
        "╚════════════════════════════════════════════════════════════════╝"


-- CATEGORIAS CON MAS GASTO
-- Funcion que pide la fecha para mostrar la info
generarCategoriasMayorGastoIO :: EstadoSistema -> IO ()
generarCategoriasMayorGastoIO estado = do
    putStrLn ""
    putStrLn "--- CATEGORIAS CON MAYOR GASTO ---"
    putStrLn "(Especificar mes y año para filtrar)"
    
    m <- pedirNumeroEnRango "Mes (1-12, 0 para todos): " 0 12
    a <- pedirNumeroEnRango "Año (0 para todos): " 0 2100
    
    let registrosFiltrados = 
            if m == 0 && a == 0 
                then registros estado
            else if m == 0
                then filter (\r -> anio (fecha r) == a) (registros estado)
            else if a == 0
                then filter (\r -> mes (fecha r) == m) (registros estado)
            else filtrarPorMes m a (registros estado)
    
    let gastosFiltrables = filtrarPorTipo Gasto registrosFiltrados
    
    if null gastosFiltrables
        then putStrLn "\nNo hay gastos para el periodo especificado."
        else do
            let periodo = if m == 0 && a == 0 
                         then "Todos los periodos"
                         else if m == 0 then "Año " ++ show a
                              else if a == 0 then nombreDelMes m ++ " (todos los años)"
                              else nombreDelMes m ++ " " ++ show a
            putStrLn (generarCategoriasMayorGasto periodo gastosFiltrables)


-- Genera el reporte con porcentajes
generarCategoriasMayorGasto :: String -> [RegistroFinanciero] -> String
generarCategoriasMayorGasto periodo registros =
    let categorias = nub (map categoria registros)
        
        -- Calcular total por categoría
        totalesPorCategoria = map (\cat -> 
            (cat, totalMonto (filter (\r -> map toLower (categoria r) == map toLower cat) registros))
            ) categorias
        
        -- Ordenar de mayor a menor
        ordenados = sortBy (\(_, m1) (_, m2) -> compare m2 m1) totalesPorCategoria
        
        -- Tomar top 10
        top10 = take 10 ordenados
        
        -- Calcular porcentaje del total
        totalGastos = totalMonto registros
        
        -- Generar líneas del reporte
        lineas = map (\(cat, monto) ->
            let pct = if totalGastos > 0 then (monto / totalGastos) * 100 else 0
                barra = generarBarra (round (pct / 2)) -- Barra de 50 caracteres max
            in
                "║ " ++ padRight 15 cat ++ " │ " ++ 
                barra ++ 
                " " ++ show (round pct) ++ "%"
            ) top10
    in
        "\n╔═══════════════════════════════════════════════════════════════╗\n" ++
        "║   GASTO POR CATEGORIA: " ++ padRight 30 periodo ++ "║\n" ++
        "╠═══════════════════════════════════════════════════════════════╣\n" ++
        intercalate "\n" lineas ++ "\n" ++
        "╠═══════════════════════════════════════════════════════════════╣\n" ++
        "║ Total de gastos: " ++ padRight 45 (formatoMoneda totalGastos) ++ "║\n" ++
        "╚═══════════════════════════════════════════════════════════════╝"


-- Funciones auxiliares para el menu y mostrar cosas

-- Total por tipo (función local para reportes)
totalPorTipo' :: TipoRegistro -> [RegistroFinanciero] -> Double
totalPorTipo' t = totalMonto . filtrarPorTipo t

-- Formato moneda
formatoMoneda :: Double -> String
formatoMoneda m = "$" ++ show (round m :: Integer)

-- Rellena un string a la derecha con espacios (máximo 20 caracteres)
pad20 :: String -> String
pad20 s = take 26 (s ++ repeat ' ')

-- Rellena a la derecha con 25 caracteres
pad25 :: String -> String
pad25 s = take 25 (s ++ repeat ' ')

-- Rellena a la derecha con 30 caracteres
pad30 :: String -> String
pad30 s = take 30 (s ++ repeat ' ')

-- Rellena un string a la derecha (máximo n caracteres)
pad :: Int -> String -> String
pad n s = take n (s ++ repeat ' ')

-- Rellena a la derecha con espacios
padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

-- Rellena a la izquierda con espacios
padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s

-- Genera una barra visual para porcentajes
generarBarra :: Int -> String
generarBarra n = replicate (max 0 (min n 25)) '█' ++ replicate (max 0 (25 - n)) '░'

-- Obtiene el nombre del mes
nombreDelMes :: Int -> String
nombreDelMes m = case m of
    1  -> "Enero"
    2  -> "Febrero"
    3  -> "Marzo"
    4  -> "Abril"
    5  -> "Mayo"
    6  -> "Junio"
    7  -> "Julio"
    8  -> "Agosto"
    9  -> "Septiembre"
    10 -> "Octubre"
    11 -> "Noviembre"
    12 -> "Diciembre"
    _  -> "Mes invalido"

-- Función para pedir número en rango (igual que en Registro.hs)
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