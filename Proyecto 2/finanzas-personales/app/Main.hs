-- ============================================================
-- Main.hs
-- Punto de entrada del sistema de finanzas personales
--
-- Este archivo es el que ejecuta Haskell primero.
-- Su trabajo es:
--   1. Cargar los datos guardados (o iniciar desde cero)
--   2. Mostrar el menú principal
--   3. Llamar al módulo correcto según lo que elija el usuario
--   4. Guardar los datos antes de salir
--
-- Depende de: Types.hs, Registro.hs, Persistencia.hs
--             (y eventualmente Presupuesto, Analisis, Reportes)
-- ============================================================

module Main where

import Types
import Registro
import Persistencia
import Presupuesto 


-- ============================================================
-- FUNCIÓN PRINCIPAL
--
-- 'main' es la función que Haskell ejecuta al iniciar.
-- Siempre tiene tipo IO ().
-- ============================================================

main :: IO ()
main = do
    -- Mensaje de bienvenida
    putStrLn ""
    putStrLn "╔══════════════════════════════════════════╗"
    putStrLn "║    SISTEMA DE FINANZAS PERSONALES        ║"
    putStrLn "║         Proyecto 2 - Haskell             ║"
    putStrLn "╚══════════════════════════════════════════╝"
    putStrLn ""

    -- Cargar el estado desde el archivo (o estado vacío si es la primera vez)
    estadoInicial <- cargarEstado

    -- Entrar al menú principal con el estado cargado
    estadoFinal <- menuPrincipal estadoInicial

    -- Guardar el estado final antes de cerrar
    guardarEstado estadoFinal
    putStrLn "¡Hasta luego!"


-- ============================================================
-- MENÚ PRINCIPAL
-- ============================================================

menuPrincipal :: EstadoSistema -> IO EstadoSistema
menuPrincipal estado = do
    putStrLn ""
    putStrLn "╔══════════════════════════════════════════╗"
    putStrLn "║              MENÚ PRINCIPAL              ║"
    putStrLn "╠══════════════════════════════════════════╣"
    putStrLn "║  1. Registros financieros                ║"
    putStrLn "║  2. Presupuestos y reglas  (Persona 2)   ║"
    putStrLn "║  3. Análisis y simulación  (Persona 3)   ║"
    putStrLn "║  4. Reportes               (Persona 4)   ║"
    putStrLn "║  0. Guardar y salir                      ║"
    putStrLn "╚══════════════════════════════════════════╝"
    putStr "Elige una opción: "

    opcion <- getLine

    case opcion of
        "1" -> do
            nuevoEstado <- menuRegistros estado
            -- Guardamos después de cada operación en registros
            guardarEstado nuevoEstado
            menuPrincipal nuevoEstado
        "2" -> do
            putStrLn "(Módulo de Presupuestos - Persona 2)"
            nuevoEstado <- menuPresupuestos estado
            menuPrincipal nuevoEstado
           
        "3" -> do
            putStrLn "(Módulo de Análisis - Persona 3)"
            menuPrincipal estado
        "4" -> do
            putStrLn "(Módulo de Reportes - Persona 4)"
            menuPrincipal estado
        "0" ->
            return estado
        _   -> do
            putStrLn "Opción no válida."
            menuPrincipal estado
