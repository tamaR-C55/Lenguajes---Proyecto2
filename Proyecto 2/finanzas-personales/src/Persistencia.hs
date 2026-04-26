-- Módulo de persistencia 
-- Este módulo se encarga de:
--   Guardar el EstadoSistema completo en un archivo
--   Cargar el EstadoSistema desde el archivo al iniciar
--   Crear el archivo si no existe (primera vez)
--   Manejar errores de lectura sin que el programa se caiga


--   Usamos 'show' para convertir el estado a texto y
--   'read' para reconstruirlo. Esto funciona gracias al 'deriving (Show, Read)' que pusimos en Types.hs.

--   El archivo guarda el EstadoSistema completo, que incluye:
--     Lista de registros financieros
--     Lista de presupuestos
--     Lista de reglas del sistema
{- HLINT ignore "Redundant return" -}

module Persistencia where

-- SECCIÓN 1: IMPORTACIONES
-- ============================================================

import Types

-- System.IO nos da control sobre archivos: abrir, cerrar,
-- leer,escribir, y manejar el encoding del texto

import System.IO

-- System.Directory nos permite verificar si un archivo
-- o carpeta existe antes de intentar leerlo
import System.Directory (doesFileExist, createDirectoryIfMissing)

-- Control.Exception nos permite capturar errores en tiempo
-- de ejecución (como archivo corrupto) sin que el programa
-- se cierre abruptamente
import Control.Exception (catch, SomeException, evaluate)

-- SECCIÓN 2: CONFIGURACIÓN DE RUTAS

-- Definimos las rutas del archivo y la carpeta como
-- constantes para poder cambiarlas fácilmente en un
-- solo lugar si fuera necesario.
-- ============================================================

-- Carpeta donde se guardan los archivos de datos
carpetaDatos :: FilePath
carpetaDatos = "data"
-- FilePath es simplemente un alias de String en Haskell,
-- lo usamos para dejar claro que es una ruta de archivo.

-- Ruta completa del archivo de persistencia
archivoEstado :: FilePath
archivoEstado = carpetaDatos ++ "/registros.txt"

-- SECCIÓN 3: GUARDAR EL ESTADO EN ARCHIVO

-- Convierte el EstadoSistema completo a texto con 'show'y lo escribe en el archivo.

-- 'show estado' produce un String que representa el estado
-- completo, incluyendo todos los registros, presupuestos
-- y reglas. Ese texto puede ser leído de vuelta con 'read'.
-- ============================================================

guardarEstado :: EstadoSistema -> IO ()
-- IO () → hace entrada/salida pero no devuelve valor útil
guardarEstado estado = do

    -- Paso 1: Crear la carpeta "data/" si no existe todavía
    -- True = crear carpetas intermedias si hacen falta
    createDirectoryIfMissing True carpetaDatos

    -- Paso 2: Abrir el archivo para escritura
    -- WriteMode borra el contenido anterior y escribe desde cero
    -- Esto es correcto porque guardamos el estado COMPLETO cada vez
    handle <- openFile archivoEstado WriteMode
    -- 'handle' es como un "puntero" al archivo abierto.
    -- Todas las operaciones de escritura van a través de él.

    -- Paso 3: Establecer el encoding a UTF-8
    -- Importante para que los caracteres especiales se guarden correctamente
    hSetEncoding handle utf8

    -- Paso 4: Escribir el estado como texto en el archivo
    hPutStrLn handle (show estado)
    -- 'show estado' convierte el EstadoSistema completo a String.
    
    -- hPutStrLn escribe texto en el archivo con salto de línea.

    -- Paso 5: Cerrar el archivo
    hClose handle

    putStrLn "Datos guardados correctamente."


-- SECCIÓN 4: CARGAR EL ESTADO DESDE ARCHIVO

-- Lee el archivo y reconstruye el EstadoSistema con 'read'.
-- Si el archivo no existe o está corrupto, devuelve
-- un estado vacío para que el programa pueda arrancar.
-- ============================================================

cargarEstado :: IO EstadoSistema
cargarEstado = do

    -- Paso 1: Verificar si el archivo existe
    existe <- doesFileExist archivoEstado
    -- doesFileExist devuelve IO Bool
    -- '<-' extrae el Bool del IO para usarlo en el if

    if not existe
        then do
            -- Primera vez que se ejecuta el programa:
            -- no hay archivo todavía, empezamos con estado vacío
            putStrLn "Archivo de datos no encontrado."
            putStrLn "Iniciando con un sistema nuevo."
            return estadoVacio
            -- 'return' envuelve estadoVacio en IO para devolverlo

        else do
            -- El archivo existe, intentamos cargarlo
            resultado <- cargarConManejo
            return resultado


-- Función auxiliar que carga el archivo y maneja posibles errores
-- Separamos esto para poder usar 'catch' con más claridad
cargarConManejo :: IO EstadoSistema
cargarConManejo = do

    -- Abrimos el archivo en modo lectura
    handle <- openFile archivoEstado ReadMode

    -- Establecemos UTF-8 para leer bien los caracteres especiales
    hSetEncoding handle utf8

    -- Leemos todo el contenido del archivo como un String
    contenido <- hGetContents handle
    -- hGetContents lee el archivo completo de una vez.
    -- Haskell usa "lazy evaluation" (evaluación perezosa), lo que significa que el contenido no se lee realmente
    -- hasta que lo necesitemos. Por eso usamos 'evaluate' abajo para forzar la lectura antes de cerrar el archivo.

    -- Forzamos la evaluación completa del contenido antes de cerrar el archivo 
    contenidoForzado <- evaluate (length contenido) >>
                        return contenido
    -- 'evaluate (length contenido)' fuerza que todo el String
    -- sea leído desde el disco. Luego 'return contenido' lo devuelve para que podamos usarlo.

    -- Cerramos el archivo
    hClose handle

    -- Intentamos convertir el texto de vuelta a EstadoSistema
    -- usando 'read', que es el inverso de 'show'
    catch
        (do
            -- 'reads' es más seguro que 'read':
            -- devuelve [(valor, restoDelString)] en vez de fallar
            case reads contenidoForzado :: [(EstadoSistema, String)] of
                [(estado, _)] -> do
                    -- Parseo exitoso
                    let n = length (registros estado)
                    putStrLn ("Datos cargados: " ++ show n ++
                              " registro(s) encontrado(s).")
                    return estado
                _ -> do
                    -- El archivo existe pero el formato no es válido
                    putStrLn "Advertencia: el archivo de datos está en un"
                    putStrLn "formato no reconocido. Iniciando desde cero."
                    return estadoVacio)

        -- Si ocurre CUALQUIER excepción durante la lectura,
        -- capturamos el error aquí y devolvemos estado vacío
        (\e -> do
            let _ = e :: SomeException
            -- Le decimos a Haskell que 'e' es de tipo SomeException
            -- para que el compilador sepa qué tipo de error capturamos
            putStrLn "Error al leer el archivo de datos."
            putStrLn "Es posible que el archivo esté dañado."
            putStrLn "Iniciando con un sistema nuevo."
            return estadoVacio)


-- SECCIÓN 5: GUARDADO AUTOMÁTICO

-- Esta función envuelve cualquier operación que modifique
-- el estado y guarda automáticamente al terminar.
-- La usan los otros módulos para no tener que acordarse de llamar a guardarEstado manualmente.
-- ============================================================

-- Ejecuta una acción que modifica el estado y guarda el resultado
-- recibe el estado actual y una función que lo transforma,
-- guarda el nuevo estado y lo devuelve
guardarTrasAccion :: EstadoSistema -> IO EstadoSistema -> IO EstadoSistema
guardarTrasAccion _ accion = do
    nuevoEstado <- accion
    -- Ejecutamos la acción que modifica el estado
    guardarEstado nuevoEstado
    -- Guardamos el resultado automáticamente
    return nuevoEstado
    -- Devolvemos el nuevo estado para que el programa lo use


-- SECCIÓN 6: UTILIDADES DE ARCHIVO
-- ============================================================

-- Verifica si ya existe un archivo de datos guardado
-- Útil para mostrar mensajes distintos al iniciar el programa
hayDatosGuardados :: IO Bool
hayDatosGuardados = doesFileExist archivoEstado


-- Muestra información sobre el archivo guardado
infoArchivo :: IO ()
infoArchivo = do
    existe <- doesFileExist archivoEstado
    if not existe
        then putStrLn "No hay archivo de datos guardado todavía."
        else do
            handle   <- openFile archivoEstado ReadMode
            hSetEncoding handle utf8
            contenido <- hGetContents handle
            _         <- evaluate (length contenido)
            hClose handle
            putStrLn ("Archivo: " ++ archivoEstado)
            putStrLn ("Tamaño:  " ++ show (length contenido) ++ " caracteres")


-- SECCIÓN 7: EXPLICACIÓN DE CÓMO FUNCIONA show/read

-- Esta sección es solo comentario educativo para entender
-- la estrategia de persistencia que usamos.

-- Cuando llamamos: guardarEstado miEstado
--
-- 'show miEstado' produce algo así en el archivo:
--
--   EstadoSistema {
--     registros = [
--       RegistroFinanciero {
--         registroId = 1,
--         tipo = Gasto,
--         monto = 15000.0,
--         categoria = "Alimentacion",
--         fecha = Fecha {dia = 15, mes = 3, anio = 2025},
--         descripcion = "Supermercado",
--         etiquetas = ["variable","mensual"]
--       }
--     ],
--     presupuestos = [],
--     reglas = []
--   }
--
-- Y cuando llamamos: cargarEstado
--
-- 'read contenido :: EstadoSistema' toma ese texto y
-- reconstruye exactamente el mismo EstadoSistema en memoria.
--
-- Esto funciona porque en Types.hs pusimos:
--   deriving (Show, Read)
-- en TODOS los tipos de datos.
-- ============================================================
