----------------------------------------------------------------
-- Modulo de simulacion de finanzas personales

-- Permite simular escenarios como reduccion de gastos en un porcentaje
-- y proyeccion de ahorro en el tiempo

----------------------------------------------------------------

-- Declaracion del modulo y las funciones que exporta
module Simulacion (
    simularReduccionGastos,
    proyeccionAhorro
) where

-- Seccion 1: IMPORTACIONES
-- ============================================================
import Types                        --Importar los tipos de datos necesarios para representar el estado del sistema, registros financieros, periodos, etc.
import Analisis (flujoCajaMensual)  -- Para calcular el flujo de caja promedio en la proyeccion de ahorro


-- =============================================================
--Funcion 1: simularReduccionGastos
-- ==============================================================
    --Aplica una reduccion porcentual a todos los gastos que ocurren
    --ddentro de un periodo especifico


    --Recibe:
    -- porcentaje: numero entre 0 y 100
    -- periodo: mes y año para aplicar la reduccion
    -- estado: el estado actual del sistema con todos los registros

    --Devuelve:
    -- Un nuevo EstadoSistema identico al original, excepto que los registros
    -- de tipo Gasto con fecha dentro del periodo, se multiplican por (1 - porcentaje/100)

    --El estado original no s emodifica, solo se crea una nueva version con los cambios aplicados para la simulacion

-- ==============================================================   

--Toma un porcenatje, un periodo y el estado actual del sistema, y devuelve un nuevo estado
simularReduccionGastos :: Double -> Periodo -> EstadoSistema -> EstadoSistema


simularReduccionGastos porcentaje (Periodo mes anio) estado =
    --Se deben de preservar  presupuestos y reglas, solo se modifican los registros
    -- Se mapea cada registro con la funcion modificarRegistro, que decide si se modifica o no segun el tipo y fecha del registro
    estado {registros = map modificarRegistro (registros estado)} 
    where

        --Funcion que transforma un unico resgitro
        modificarRegistro :: RegistroFinanciero -> RegistroFinanciero
        modificarRegistro reg
            -- Si el registro es un gasto que ocurre en el mismo mes y año del periodo, se aplica la reduccion
            | esGastoEnPeriodo reg = reg { monto = monto reg * (1 - porcentaje / 100) }
            | otherwise = reg   -- Si no cumple las condiciones, se devuelve el registro sin cambios

        --Determina si un regsitro debe ser modificado
        -- Debe de ser de tipo gasto
        -- La fecha debe pertencer al mismo mes y año del periodo
        esGastoEnPeriodo :: RegistroFinanciero -> Bool
        esGastoEnPeriodo reg = 
            tipo reg == Gasto && mismoMesAnio (fecha reg) mes anio                                  --mismoMesAnio esta definida en Types.hs y compara la fecha del registro con el mes y año del periodo


-- =============================================================
-- Funcion 2: proyeccionAhorro
-- ==============================================================
    -- Proyecta el ahorro acumulado mes a mes durante 'n' meses,
    -- partiendo de un periodo de inicio dado.
    --
    -- Primero calcula el flujo de caja promedio de los
    -- 3 meses anteriores al periodo de inicio.
    -- Luego asume que ese flujo promedio se repite cada mes y lo
    -- acumula sucesivamente.
    --
    -- Recibe:
    --   Periodo: mes y año desde el cual comenzar la proyeccion
    --   Int: numero de meses a proyectar (n)
    --   EstadoSistema: estado actual con todos los registros financieros
    --
    -- Devuelve:
    --   Una lista de montos con los ahorros acumulados
    --   al final de cada mes proyectado.
    --   Ejemplo: si n = 3 y flujoPromedio = 100, devuelve [100, 200, 300]
    --
    -- Notas:
    --   - Si n == 0 o n < 0, devuelve lista vacia.
    --   - Si no hay suficientes meses historicos,
    --     se calcula el promedio con los que existan, o 0 si no hay ninguno.
    --   - La funcion depende de 'flujoCajaMensual', del modulo Analisis
-- ==============================================================

-- Tipo: periodo de inicio, numero de meses a proyectar, estado del sistema 
-- Devuelve una lista de montos
proyeccionAhorro :: Periodo -> Int -> EstadoSistema -> [Monto]

-- Si n es 0, no se proyecta nada y se devuelve una lista vacia
proyeccionAhorro _ 0 _ = []

--Caso general
proyeccionAhorro (Periodo mesInicio anioInicio) n estado
    | n < 0 = []                                                                                    --Si n es negativo, se devuelve lista vacia
    | otherwise =
        let flujoPromedio = flujoPromedioHistorico (Periodo mesInicio anioInicio) estado
                                                                                                    -- Se genera una lista de ahorros acumulados comenzando en 0
                                                                                                    --luego toma los primeros n elementos despues del primer 0
                                                                                                    -- iterate aplica la funcion una y otra vez sobre el valor anterior
                                                                                                    -- tail omite el primer 0, y take n toma los siguientes n valores.
            ahorrosAcumulados = take n (tail (iterate (+ flujoPromedio) 0))    
        in ahorrosAcumulados
  where


    -- Calcula el flujo de caja promedio de los 3 meses anteriores al periodo dado
    flujoPromedioHistorico (Periodo m a) est =

        
        let mesesPrevios = tomarMesesAnteriores (m, a) 3                                            -- Primero obtiene cuales son los 3 meses anteriores al periodo (m, a) y los guarda en una lista de tuplas (mes, año)
            flujos = map (\ (mm, aa) -> flujoCajaMensual (Periodo mm aa) est) mesesPrevios          -- Calcula el flujo de caja mensual para cada uno de esos meses usando la funcion flujoCajaMensual del modulo Analisis
            suma = sum flujos                                                                       -- Suma los flujos obtenidos
            cantidad = length flujos                                                                -- Cuenta cuantos flujos se calcularon (puede ser menos de 3)
        in if cantidad == 0 then 0 else suma / fromIntegral cantidad                                -- Devuelve el promedio, manejando el caso de no tener meses previos (division por cero)


    --Toma los k meses anteriores a un mes y año dado, devolviendo una lista de tuplas (mes, año)
    tomarMesesAnteriores _ 0 = []
    tomarMesesAnteriores (m, a) k =
        let ant = mesAnterior (m, a)
        in ant : tomarMesesAnteriores ant (k-1)


    --Dado un mes y año, devuelve el mes y año del mes anterior, manejando el cambio de año
    mesAnterior (1, a) = (12, a - 1)
    mesAnterior (m, a) = (m - 1, a)