---------------------------------------------------------------------
-- Modulo de análisis de finanzas personales
--Proporciona funciones para calcular flujo de caja, tendencias
--proyecciones e identificacion de categorias con mayor impacto
---------------------------------------------------------------------

module Analisis (
    flujoCajaMensual,
    tendenciaGastosPromedio,
    proyectarGastosMes,
    categoriasMayorGasto,
    porcentajeGastoPorCategoria
) where


--Importaciones necesarias
import Types
import Data.List(sortBy, groupBy)   -- Para ordenar listas
import Data.Function(on)   --Para ordenar por monto
import System.IO (hFlush, stdout)  -- Para limpiar el buffer de salida en el menú



----------------------------------------------------------------
-- Funcion 1: flujoCajamensual
    -- Calcula el flujo de caja neto para un periodo especifico (mes y año)
    -- Flijo de caja = total ingresos - total gastos
    --No se incluyen registros de tipo Ahorro o Inversion

    --Parametros
        --periodo: mes y año para calcular el flujo
        --estado: el estado actual del sistema con todos los registros

    --Devuelve:
        --El flujo de caja Monto como un numero Double
        --Puede serpositivo o negativo

flujoCajaMensual :: Periodo -> EstadoSistema -> Monto
flujoCajaMensual (Periodo mes anio) estado = 
    let registrosEnPeriodo = filter (estaEnPeriodo mes anio) (registros estado)
        ingresos = sum [monto r | r <-registrosEnPeriodo, tipo r == Ingreso]
        gastos = sum [monto r | r <-registrosEnPeriodo, tipo r == Gasto]
    in ingresos - gastos
    where
        --Funcion auxiliar para determinar si un registro esta en el periodo
        estaEnPeriodo :: Int -> Int -> RegistroFinanciero -> Bool
        estaEnPeriodo m a reg = mismoMesAnio (fecha reg) m a



------------------------------------------------------------------
-- Función 2 CORREGIDA: tendenciaGastosPromedio
tendenciaGastosPromedio :: Periodo -> Int -> EstadoSistema -> [(Categoria, Monto)]
tendenciaGastosPromedio (Periodo mesInicio anioInicio) n estado
    | n <= 0    = []
    | otherwise =
        let periodos         = tomarPeriodos (mesInicio, anioInicio) n
            gastosEnRango    = filter (esGastoAlgunPeriodo periodos) (registros estado)
            sumaPorCategoria = agregarPorCategoria gastosEnRango
            promedio         = [(cat, total / fromIntegral n) | (cat, total) <- sumaPorCategoria]
        in promedio   
    where
        
        esGastoAlgunPeriodo :: [(Int, Int)] -> RegistroFinanciero -> Bool
        esGastoAlgunPeriodo ps reg = tipo reg == Gasto && any (estaEnPeriodo reg) ps

        estaEnPeriodo :: RegistroFinanciero -> (Int, Int) -> Bool
        estaEnPeriodo reg (m, a) = mismoMesAnio (fecha reg) m a

        tomarPeriodos :: (Int, Int) -> Int -> [(Int, Int)]
        tomarPeriodos (m, a) 1 = [(m, a)]
    
        tomarPeriodos (m, a) k = (m, a) : tomarPeriodos (siguienteMes (m, a)) (k - 1)


        siguienteMes :: (Int, Int) -> (Int, Int)
        siguienteMes (12, a) = (1, a + 1)
        siguienteMes (m,  a) = (m + 1, a)

        agregarPorCategoria :: [RegistroFinanciero] -> [(Categoria, Monto)]
        agregarPorCategoria = foldr agregar []
            where
                agregar reg [] = [(categoria reg, monto reg)]
                agregar reg ((cat, total):rest)
                    | categoria reg == cat = (cat, total + monto reg) : rest
                    | otherwise            = (cat, total) : agregar reg rest

-------------------------------------------------------------------------------------

-- Función 3: proyectarGastosMes
proyectarGastosMes :: Periodo -> EstadoSistema -> [(Categoria, Monto)]
proyectarGastosMes (Periodo mes anio) estado =
    let mesesAnteriores = tomarMesesAnteriores (mes, anio) 3
        gastos          = concatMap (gastosDelMes estado) mesesAnteriores

        
        gastosOrdenados = sortBy (compare `on` fst) gastos

        
        grupos = groupBy ((==) `on` fst) gastosOrdenados

        
        sumaPorCat = map (\g -> (fst (head g), sum (map snd g))) grupos

        cantidadMeses = length mesesAnteriores
        promedios = [(cat, total / fromIntegral cantidadMeses) | (cat, total) <- sumaPorCat]
    in promedios
    where
        tomarMesesAnteriores :: (Int, Int) -> Int -> [(Int, Int)]
        tomarMesesAnteriores _ 0 = []
        tomarMesesAnteriores (m, a) k =
            let ant = mesAnterior (m, a)
            in ant : tomarMesesAnteriores ant (k - 1)

        mesAnterior :: (Int, Int) -> (Int, Int)
        mesAnterior (1, a) = (12, a - 1)
        mesAnterior (m, a) = (m - 1, a)

        gastosDelMes :: EstadoSistema -> (Int, Int) -> [(Categoria, Monto)]
        gastosDelMes est (m, a) =
            let registrosMes = filter (\r -> tipo r == Gasto && mismoMesAnio (fecha r) m a)
                                      (registros est)
            in map (\r -> (categoria r, monto r)) registrosMes

-- =============================================================
-- Función 4: categoriasMayorGasto
-- =============================================================
categoriasMayorGasto :: Periodo -> EstadoSistema -> [(Categoria, Monto)]
categoriasMayorGasto (Periodo mes anio) estado =
    let gastosDelMes = filter (\r -> tipo r == Gasto && mismoMesAnio (fecha r) mes anio)
                              (registros estado)
        sumaPorCat = foldr acumular [] gastosDelMes
        ordenados = sortBy (flip compare `on` snd) sumaPorCat
    in ordenados
  where
    acumular reg [] = [(categoria reg, monto reg)]
    acumular reg ((cat, total):rest)
        | categoria reg == cat = (cat, total + monto reg) : rest
        | otherwise            = (cat, total) : acumular reg rest

-- =============================================================
-- Función 5: porcentajeGastoPorCategoria
-- =============================================================
porcentajeGastoPorCategoria :: Periodo -> EstadoSistema -> [(Categoria, Double)]
porcentajeGastoPorCategoria periodo estado =
    let catMontos = categoriasMayorGasto periodo estado
        totalGasto = sum (map snd catMontos)
    in if totalGasto == 0
       then []
       else map (\(cat, monto) -> (cat, (monto / totalGasto) * 100)) catMontos
