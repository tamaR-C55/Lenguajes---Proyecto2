-- ============================================================
-- MenuAnalisis.hs
-- Interfaz de usuario para el módulo de análisis y simulación
--
-- Este módulo maneja toda la interacción con el usuario para
-- las funciones de análisis y simulación. Importa ambos módulos
-- sin generar dependencia circular.
--
-- Dependencias:
--   Types      → tipos de datos
--   Analisis   → funciones puras de análisis
--   Simulacion → funciones puras de simulación
-- ============================================================

module MenuAnalisis (menuAnalisis) where

import System.IO (hFlush, stdout)
import Types
import Analisis
import Simulacion


-- ============================================================
-- MENÚ PRINCIPAL DE ANÁLISIS
-- ============================================================

menuAnalisis :: EstadoSistema -> IO EstadoSistema
menuAnalisis estado = do
    putStrLn ""
    putStrLn "╔══════════════════════════════════════════════════╗"
    putStrLn "║            ANÁLISIS Y SIMULACIÓN                 ║"
    putStrLn "╠══════════════════════════════════════════════════╣"
    putStrLn "║  1. Flujo de caja mensual                        ║"
    putStrLn "║  2. Tendencia de gastos promedio (últimos N meses)║"
    putStrLn "║  3. Proyección de gastos para un mes             ║"
    putStrLn "║  4. Categorías con mayor gasto en un período     ║"
    putStrLn "║  5. Porcentaje de gasto por categoría            ║"
    putStrLn "║  6. Simular reducción de gastos (% en un mes)    ║"
    putStrLn "║  7. Proyección de ahorro acumulado               ║"
    putStrLn "║  0. Volver al menú principal                     ║"
    putStrLn "╚══════════════════════════════════════════════════╝"
    putStr "Elige una opción: "
    hFlush stdout
    opcion <- getLine

    case opcion of
        "1" -> do
            periodo <- leerPeriodo
            let flujo = flujoCajaMensual periodo estado
            putStrLn "\n╔══════════════════════════════════════╗"
            putStrLn $ "║ Flujo de caja: " ++ show flujo
            putStrLn "╚══════════════════════════════════════╝"
            esperarTecla
            menuAnalisis estado

        "2" -> do
            periodo <- leerPeriodo
            putStr "Cantidad de meses consecutivos a considerar: "
            hFlush stdout
            nStr <- getLine
            let n = read nStr :: Int
            let tendencias = tendenciaGastosPromedio periodo n estado
            putStrLn "\n╔══════════════════════════════════════════════════════╗"
            putStrLn "║   Categoría          Gasto promedio mensual          ║"
            putStrLn "╠══════════════════════════════════════════════════════╣"
            if null tendencias
                then putStrLn "║   No hay datos en ese período.                       ║"
                else mapM_ mostrarPar tendencias
            putStrLn "╚══════════════════════════════════════════════════════╝"
            esperarTecla
            menuAnalisis estado

        "3" -> do
            periodo <- leerPeriodo
            let proyeccion = proyectarGastosMes periodo estado
            putStrLn "\n╔══════════════════════════════════════════════════════╗"
            putStrLn "║   Proyección de gastos para el mes (promedio 3 meses)║"
            putStrLn "╠══════════════════════════════════════════════════════╣"
            if null proyeccion
                then putStrLn "║   No hay datos históricos suficientes.               ║"
                else mapM_ mostrarPar proyeccion
            putStrLn "╚══════════════════════════════════════════════════════╝"
            esperarTecla
            menuAnalisis estado

        "4" -> do
            periodo <- leerPeriodo
            let ranking = categoriasMayorGasto periodo estado
            putStrLn "\n╔══════════════════════════════════════════════════════╗"
            putStrLn "║   Categorías con mayor gasto en el período           ║"
            putStrLn "╠══════════════════════════════════════════════════════╣"
            if null ranking
                then putStrLn "║   No hay gastos en este período.                     ║"
                else mapM_ mostrarPar ranking
            putStrLn "╚══════════════════════════════════════════════════════╝"
            esperarTecla
            menuAnalisis estado

        "5" -> do
            periodo <- leerPeriodo
            let porcentajes = porcentajeGastoPorCategoria periodo estado
            putStrLn "\n╔══════════════════════════════════════════════════════╗"
            putStrLn "║   % del gasto total por categoría                    ║"
            putStrLn "╠══════════════════════════════════════════════════════╣"
            if null porcentajes
                then putStrLn "║   No hay gastos en este período.                     ║"
                else mapM_ mostrarPorcentaje porcentajes
            putStrLn "╚══════════════════════════════════════════════════════╝"
            esperarTecla
            menuAnalisis estado

        "6" -> do
            periodo <- leerPeriodo
            putStr "Porcentaje de reducción (0-100): "
            hFlush stdout
            porStr <- getLine
            let porcentaje = read porStr :: Double
            let estadoSimulado = simularReduccionGastos porcentaje periodo estado
            putStrLn "\n╔══════════════════════════════════════════════════════╗"
            putStrLn "║      Simulación: gastos reducidos en el período      ║"
            putStrLn "╚══════════════════════════════════════════════════════╝"
            let cambios = filter (\(r1, r2) -> monto r1 /= monto r2)
                                 (zip (registros estado) (registros estadoSimulado))
            if null cambios
                then putStrLn "No se modificó ningún gasto (puede que no haya gastos en el período)."
                else mapM_ (\(rOrig, rNuevo) ->
                        putStrLn $ "  ID " ++ show (registroId rOrig) ++
                                   " : " ++ show (monto rOrig) ++
                                   " → " ++ show (monto rNuevo))
                        cambios
            putStrLn "\n(Nota: esta simulación no se guarda, solo se muestra el efecto)"
            esperarTecla
            menuAnalisis estado

        "7" -> do
            periodo <- leerPeriodo
            putStr "Número de meses a proyectar: "
            hFlush stdout
            nStr <- getLine
            let n = read nStr :: Int
            let proyeccionAhorros = proyeccionAhorro periodo n estado
            putStrLn "\n╔══════════════════════════════════════════════════════╗"
            putStrLn "║   Proyección de ahorro acumulado mes a mes           ║"
            putStrLn "╠══════════════════════════════════════════════════════╣"
            if null proyeccionAhorros
                then putStrLn "║   No se pudo proyectar (sin datos históricos?)       ║"
                else mapM_ (\(i, m) ->
                        putStrLn $ "║   Mes " ++ show i ++ ": " ++ show m)
                        (zip [1..] proyeccionAhorros)
            putStrLn "╚══════════════════════════════════════════════════════╝"
            esperarTecla
            menuAnalisis estado

        "0" -> return estado

        _ -> do
            putStrLn "Opción no válida, presiona Enter para continuar"
            esperarTecla
            menuAnalisis estado


-- ============================================================
-- FUNCIONES AUXILIARES
-- ============================================================

leerPeriodo :: IO Periodo
leerPeriodo = do
    putStr "Mes (1-12): "
    hFlush stdout
    mesStr <- getLine
    putStr "Año (ej: 2025): "
    hFlush stdout
    anioStr <- getLine
    return (Periodo (read mesStr :: Int) (read anioStr :: Int))

esperarTecla :: IO ()
esperarTecla = do
    putStr "\nPresiona Enter para continuar..."
    hFlush stdout
    _ <- getLine
    return ()

mostrarPar :: (String, Double) -> IO ()
mostrarPar (cat, m) =
    putStrLn $ "║   " ++ take 22 (cat ++ repeat ' ') ++ show m

mostrarPorcentaje :: (String, Double) -> IO ()
mostrarPorcentaje (cat, pct) =
    putStrLn $ "║   " ++ take 22 (cat ++ repeat ' ')
                      ++ show (fromIntegral (round (pct * 10)) / 10.0 :: Double) ++ "%"