----------------------------------------------------------------
-- Modulo de simulación de finanzas personales

-- Permite simular escenarios como reduccion de gastos en un porcentaje
-- y proyeccion de ahorro en el tiempo

----------------------------------------------------------------


module Simulacion (
    simularReduccionGastos,
    proyeccionAhorro
) where

-- SECCIÓN 1: IMPORTACIONES
-- ============================================================
import Types 
import Analisis (flujoCajaMensual)  -- Para calcular el flujo de caja promedio en la proyección de ahorro


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

simularReduccionGastos :: Double -> Periodo -> EstadoSistema -> EstadoSistema
simularReduccionGastos porcentaje (Periodo mes anio) estado =
    --Se deben de preservar  presupuestos y reglas, solo se modifican los registros
    estado {registros = map modificarRegistro (registros estado)}
    where

        --Funcion que transforma un unico resgitro
        modificarRegistro :: RegistroFinanciero -> RegistroFinanciero
        modificarRegistro reg
            | esGastoEnPeriodo reg = reg { monto = monto reg * (1 - porcentaje / 100) }
            | otherwise = reg

        --Determina si un regsitro debe ser modificado
        -- Debe de ser de tipo gasto
        -- La fecha debe pertencer al mismo mes y año del periodo
        esGastoEnPeriodo :: RegistroFinanciero -> Bool
        esGastoEnPeriodo reg = 
            tipo reg == Gasto && mismoMesAnio (fecha reg) mes anio


-- =============================================================
-- proyeccionAhorro 
-- =============================================================
proyeccionAhorro :: Periodo -> Int -> EstadoSistema -> [Monto]
proyeccionAhorro _ 0 _ = []
proyeccionAhorro (Periodo mesInicio anioInicio) n estado
    | n < 0 = []
    | otherwise =
        let flujoPromedio = flujoPromedioHistorico (Periodo mesInicio anioInicio) estado
            ahorrosAcumulados = take n (tail (iterate (+ flujoPromedio) 0))    
        in ahorrosAcumulados
  where
    flujoPromedioHistorico (Periodo m a) est =
        let mesesPrevios = tomarMesesAnteriores (m, a) 3
            flujos = map (\ (mm, aa) -> flujoCajaMensual (Periodo mm aa) est) mesesPrevios
            suma = sum flujos
            cantidad = length flujos
        in if cantidad == 0 then 0 else suma / fromIntegral cantidad

    tomarMesesAnteriores _ 0 = []
    tomarMesesAnteriores (m, a) k =
        let ant = mesAnterior (m, a)
        in ant : tomarMesesAnteriores ant (k-1)

    mesAnterior (1, a) = (12, a - 1)
    mesAnterior (m, a) = (m - 1, a)