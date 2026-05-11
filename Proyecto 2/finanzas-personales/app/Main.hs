
-- ============================================================

module Main where

import Types
import Registro
import Persistencia
import Presupuesto
import MenuAnalisis (menuAnalisis)
import Reportes (menuReportes)
import Reglas 



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
    putStrLn "║              MENU PRINCIPAL              ║"
    putStrLn "╠══════════════════════════════════════════╣"
    putStrLn "║  1. Registros financieros                ║"
    putStrLn "║  2. Presupuestos                         ║"
    putStrLn "║  3. Analisis y simulacion                ║"
    putStrLn "║  4. Reportes                             ║"
    putStrLn "║  5. Reglas                               ║"
    putStrLn "║  0. Guardar y salir                      ║"
    putStrLn "╚══════════════════════════════════════════╝"
    putStr "Elige una opcion: "

    opcion <- getLine

    case opcion of
        "1" -> do
            nuevoEstado <- menuRegistros estado
            -- Guardamos después de cada operación en registros
            guardarEstado nuevoEstado
            menuPrincipal nuevoEstado
        "2" -> do
            nuevoEstado <- menuPresupuestos estado
            menuPrincipal nuevoEstado
           
        "3" -> do
            nuevoEstado <- menuAnalisis estado
            menuPrincipal nuevoEstado
        "4" -> do
            nuevoEstado <- menuReportes estado
            menuPrincipal nuevoEstado

        "5" -> do

            nuevoEstado <- menuReglas estado
            menuPrincipal nuevoEstado

        "0" ->
            return estado
        _   -> do
            putStrLn "Opcion no valida."
            menuPrincipal estado
