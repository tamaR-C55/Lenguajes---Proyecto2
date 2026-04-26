-- Módulo central de tipos de datos del sistema de finanzas.

-- Este archivo define TODOS los "moldes" de información que
-- usa el programa. Los otros módulos (Registro, Presupuesto,
-- Análisis, etc.) importan este archivo para saber qué forma
-- tienen los datos con los que trabajan.

module Types where

-- SECCIÓN 1: IMPORTACIONES
-- ============================================================

-- Data.List nos da funciones útiles para trabajar con listas
import Data.List (intercalate)

-- SECCIÓN 2: TIPO DE REGISTRO FINANCIERO
--
-- 4 tipos de registros:
--   Ingreso
--   Gasto
--   Ahorro
--   Inversión
--
-- Se define con data y se llama un
-- "tipo algebraico de suma" porque el tipo TipoRegistro
-- puede SER una cosa O la otra (Ingreso | Gasto | ...).
-- ============================================================

data TipoRegistro
    = Ingreso     -- Dinero que entra 
    | Gasto       -- Dinero que sale 
    | Ahorro      -- Dinero reservado para el futuro
    | Inversion   -- Dinero puesto a trabajar 
    deriving (Show, Read, Eq)
    -- 'deriving Show'  = convertir el valor a texto
    -- 'deriving Read'  = leer el valor desde texto
    --                    (necesario para guardar/cargar del archivo)
    -- 'deriving Eq'    = Haskell puede comparar dos TipoRegistro
    --                    con (==) para saber si son iguales


-- SECCIÓN 3: TIPO DE FECHA
--
-- El documento pide que cada registro tenga una fecha.
-- Estructura simple: día, mes y año como números enteros.
-- Ejemplo: Fecha 15 3 2025 = 15 de marzo del 2025
-- ============================================================

data Fecha = Fecha
    { dia  :: Int   -- ^ Día del mes  (1–31)
    , mes  :: Int   -- ^ Mes del año  (1–12)
    , anio :: Int   -- ^ Año completo (ej: 2025)
    }
    deriving (Show, Read, Eq)
    -- La sintaxis { campo :: Tipo } se llama "record syntax"
    -- y nos genera automáticamente funciones para acceder
    -- a cada campo: dia, mes, anio son funciones que
    -- reciben una Fecha y devuelven el valor del campo.
    -- Ej: dia (Fecha 15 3 2025) == 15


-- SECCIÓN 4: EL REGISTRO FINANCIERO

-- Representa un registro financiero completo con todos los campos:
--    Monto
--    Categoría
--    Fecha
--    Descripción
--    Etiquetas múltiples
-- ============================================================

data RegistroFinanciero = RegistroFinanciero
    { registroId    :: Int            -- Identificador único (1, 2, 3...)
    , tipo          :: TipoRegistro   -- Ingreso, Gasto, Ahorro o Inversión
    , monto         :: Double         -- Cantidad de dinero 
    , categoria     :: String         -- Ej: "Alimentación", "Salario", "Renta"
    , fecha         :: Fecha          -- Cuándo ocurrió el registro
    , descripcion   :: String         -- Texto libre explicando el registro
    , etiquetas     :: [String]       -- Lista de etiquetas, ej: ["fijo", "mensual"]
    }
    deriving (Show, Read, Eq)
    -- Las listas en Haskell se escriben con corchetes


-- SECCIÓN 5: TIPO DE PRESUPUESTO

-- Pesupuestos por categoría.
-- ============================================================

data Presupuesto = Presupuesto
    { presupuestoCategoria :: String   -- A qué categoría aplica
    , montoMaximo          :: Double   -- Cuánto se puede gastar como máximo
    , periodoMes           :: Int      -- Mes al que aplica (1–12)
    , periodoAnio          :: Int      -- Año al que aplica
    }
    deriving (Show, Read, Eq)


-- SECCIÓN 6: TIPO DE REGLA
-- El documento 2.5 pide un sistema de reglas:

-- CondicionRegla define el tipo de comparación que hace la regla. ReglaSistema junta la condición con el mensaje.

-- Las condiciones posibles para una regla
data CondicionRegla
    = GastoSuperaMonto String Double
    -- Gasto en cierta categoría supera cierto monto
    -- Ejemplo: GastoSuperaMonto "Entretenimiento" 50000.0
    | AhorroMenorA Double
    -- El ahorro total es menor a cierto valor
    -- Ejemplo: AhorroMenorA 100000.0
    deriving (Show, Read, Eq)

-- Una regla completa: condición + mensaje para mostrar al usuario
data ReglaSistema = ReglaSistema
    { condicion      :: CondicionRegla   -- Qué condición evaluar
    , mensajeAlerta  :: String           -- Qué mostrarle al usuario si se cumple
    }
    deriving (Show, Read, Eq)


-- SECCIÓN 7: EL ESTADO GENERAL DEL SISTEMA

-- Este tipo agrupa TODO lo que el programa necesita recordar mientras está corriendo: la lista de registros, los presupuestos y las reglas.
-- Es lo que se guarda en el archivo y se carga al iniciar.
-- ============================================================

data EstadoSistema = EstadoSistema
    { registros    :: [RegistroFinanciero]  -- Todos los registros guardados
    , presupuestos :: [Presupuesto]         -- Todos los presupuestos definidos
    , reglas       :: [ReglaSistema]        -- Todas las reglas del sistema
    }
    deriving (Show, Read, Eq)

-- Un estado vacío para iniciar el programa por primera vez
-- o cuando no existe el archivo de datos todavía
estadoVacio :: EstadoSistema
estadoVacio = EstadoSistema
    { registros    = []   -- lista vacía
    , presupuestos = []
    , reglas       = []
    }


-- SECCIÓN 8: FUNCIONES AUXILIARES DE CONVERSIÓN A TEXTO

-- Estas funciones convierten nuestros tipos a texto legible
-- para mostrárselos al usuario en la consola.
-- Están pensadas para ser bonitas y entendibles, no para guardar en archivo.
-- ============================================================

-- Convierte un TipoRegistro a texto en español
mostrarTipo :: TipoRegistro -> String
mostrarTipo Ingreso   = "Ingreso"
mostrarTipo Gasto     = "Gasto"
mostrarTipo Ahorro    = "Ahorro"
mostrarTipo Inversion = "Inversión"
-- Esto se llama "pattern matching": Haskell ve qué valor tiene el TipoRegistro y devuelve el texto correspondiente.
-- Es como un switch/case pero más elegante y seguro.


-- Convierte una Fecha a texto con formato DD/MM/AAAA
mostrarFecha :: Fecha -> String
mostrarFecha f =
    pad (dia f) ++ "/" ++ pad (mes f) ++ "/" ++ show (anio f)
  where
    -- 'where' define funciones locales, solo visibles aquí
    -- pad agrega un cero adelante si el número es menor a 10
    pad n = if n < 10 then "0" ++ show n else show n


-- Convierte una lista de etiquetas a texto separado por comas
-- Ejemplo: ["fijo", "mensual"] = "#fijo, #mensual"
mostrarEtiquetas :: [String] -> String
mostrarEtiquetas [] = "(sin etiquetas)"
mostrarEtiquetas es = intercalate ", " (map ("#"++) es)
-- 'map ("#"++) es' aplica la función ("#"++) a cada elemento
-- de la lista. ("#"++) es una función parcial que agrega "#" al inicio de cualquier String.
-- 'intercalate ", "' une todos los elementos con ", " entre ellos.


-- Muestra un RegistroFinanciero completo de forma legible
mostrarRegistro :: RegistroFinanciero -> String
mostrarRegistro r =
    "╔═══════════════════════════════════════\n" ++
    "║ ID:          " ++ show (registroId r)     ++ "\n" ++
    "║ Tipo:        " ++ mostrarTipo (tipo r)    ++ "\n" ++
    "║ Monto:       " ++ show (monto r)         ++ "\n" ++
    "║ Categoría:   " ++ categoria r             ++ "\n" ++
    "║ Fecha:       " ++ mostrarFecha (fecha r)  ++ "\n" ++
    "║ Descripción: " ++ descripcion r           ++ "\n" ++
    "║ Etiquetas:   " ++ mostrarEtiquetas (etiquetas r) ++ "\n" ++
    "╚═══════════════════════════════════════"
-- El operador (++) concatena dos Strings.
-- La función completa va uniendo línea por línea con (++).


-- SECCIÓN 9: FUNCIONES AUXILIARES DE COMPARACIÓN DE FECHAS

-- Estas funciones las necesitarán Adolfo y Victor  para analizar registros por mes/año.
-- ============================================================

-- Compara si dos fechas son del mismo mes y año
mismoMesAnio :: Fecha -> Int -> Int -> Bool
mismoMesAnio f m a = mes f == m && anio f == a
-- Recibe una fecha, un mes y un año
-- Devuelve True si coinciden, False si no
-- Ejemplo: mismoMesAnio (Fecha 15 3 2025) 3 2025 == True


-- Compara si una fecha es anterior a otra
fechaAnterior :: Fecha -> Fecha -> Bool
fechaAnterior f1 f2
    | anio f1 < anio f2 = True
    | anio f1 > anio f2 = False
    | mes  f1 < mes  f2 = True
    | mes  f1 > mes  f2 = False
    | otherwise          = dia f1 < dia f2
-- Los 'guards' (|) son como condiciones if/else encadenadas.
-- Haskell evalúa de arriba hacia abajo hasta encontrar True.
-- 'otherwise' es el caso por defecto (como el else final).
