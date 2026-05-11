---------------------------------------------------------------------
-- Modulo de analisis de finanzas personales
--Proporciona funciones para calcular flujo de caja, tendencias
--proyecciones e identificacion de categorias con mayor impacto
---------------------------------------------------------------------

-- Definicion del modulo y las funciones que exporta
module Analisis (
    flujoCajaMensual,
    tendenciaGastosPromedio,
    proyectarGastosMes,
    categoriasMayorGasto,
    porcentajeGastoPorCategoria
) where


--Importaciones necesarias
import Types
import Data.List(sortBy, groupBy)                       -- Para ordenar listas
import Data.Function(on)                                --Para ordenar por monto
import System.IO (hFlush, stdout)                       -- Para limpiar el buffer de salida en el menu



-- ================================================================================================
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
-- ================================================================================================


flujoCajaMensual :: Periodo -> EstadoSistema -> Monto                                              -- Recibe un periodo y el eatdo del sistema y devuleve un monto representando el flujo de caja neto para ese periodo
flujoCajaMensual (Periodo mes anio) estado = 
    let registrosEnPeriodo = filter (estaEnPeriodo mes anio) (registros estado)                    -- Filtra los registros del estado para obtener solo aquellos que ocurren en el mes y año del periodo
        ingresos = sum [monto r | r <-registrosEnPeriodo, tipo r == Ingreso]                       -- Suma los montos de los registros filtrados que son de tipo Ingreso
        gastos = sum [monto r | r <-registrosEnPeriodo, tipo r == Gasto]                           -- Suma los montos de los registros filtrados que son de tipo Gasto
    in ingresos - gastos                                                                           -- Resta los gastos de los ingresos para obtener el flujo de caja neto
    where
        
        estaEnPeriodo :: Int -> Int -> RegistroFinanciero -> Bool                                  --Funcion auxiliar para determinar si un registro esta en el periodo
        estaEnPeriodo m a reg = mismoMesAnio (fecha reg) m a                                       -- Usa la funcion mismoMesAnio definida en Types.hs para comparar la fecha del registro con el mes y año del periodo




-- ==============================================================
    -- Funcion 2: tendenciaGastosPromedio
    -- Calcula el gasto promedio por categoria durante un rango de meses,
    -- desde un mes inicial y durante 'n' meses consecutivos.
    --
    -- Parametros:
    --   periodo: mes y año de inicio (Periodo)
    --   n:       cantidad de meses a considerar (Int)
    --   estado:  estado actual del sistema
    --
    -- Devuelve:
    --   Una lista de pares (Categoria, Monto promedio mensual).
    --   Ejemplo: [("Comida", 150.0), ("Transporte", 80.0)]
    --
    -- Nota: si n <= 0, devuelve lista vacia.
-- ==============================================================

tendenciaGastosPromedio :: Periodo -> Int -> EstadoSistema -> [(Categoria, Monto)]                 -- Recibe un periodo de inicio, una cantidad de meses y el estado del sistema, y devuelve una lista con el gasto promedio por categoria durante ese rango de meses
tendenciaGastosPromedio (Periodo mesInicio anioInicio) n estado
    | n <= 0    = []                                                                               -- Si n es 0 o negativo, no se consideran meses y se devuelve una lista vacia                                             
    | otherwise =                                                                                  -- En caso contrario:
        let periodos         = tomarPeriodos (mesInicio, anioInicio) n                             -- Genera una lista de tuplas (mes, año) para los n meses consecutivos a partir del periodo de inicio, usando la funcion tomarPeriodos          
            gastosEnRango    = filter (esGastoAlgunPeriodo periodos) (registros estado)            -- Filtra los registros del estado para obtener solo aquellos que son gastos en alguno de los meses del rango, usando la funcion esGastoAlgunPeriodo
            sumaPorCategoria = agregarPorCategoria gastosEnRango                                   -- Suma los montos de los gastos por categoria, usando la funcion agregarPorCategoria que acumula los montos por categoria en una lista de pares (Categoria, Monto total)
            promedio         = [(cat, total / fromIntegral n) | (cat, total) <- sumaPorCategoria]  -- Calcula el gasto promedio por categoria dividiendo el total por n, y devuelve la lista de pares (Categoria, Monto promedio mensual)
        in promedio                                                                                
    where
        
        esGastoAlgunPeriodo :: [(Int, Int)] -> RegistroFinanciero -> Bool                          -- Funcion auxiliar para determinar si un registro de tipo Gasto ocurre en alguno de los meses del rango de periodos
        esGastoAlgunPeriodo ps reg = tipo reg == Gasto && any (estaEnPeriodo reg) ps               -- Verifica que el registro sea de tipo Gasto y que su fecha coincida con alguno de los meses en la lista de periodos, usando la funcion estaEnPeriodo

        estaEnPeriodo :: RegistroFinanciero -> (Int, Int) -> Bool                                  -- Funcion auxiliar para determinar si un registro ocurre en un mes y año especificos
        estaEnPeriodo reg (m, a) = mismoMesAnio (fecha reg) m a                                    -- Usa la funcion mismoMesAnio definida en Types.hs para comparar la fecha del registro con el mes y año dados

        tomarPeriodos :: (Int, Int) -> Int -> [(Int, Int)]                                         -- Funcion auxiliar para generar una lista de tuplas (mes, año) para n meses consecutivos a partir de un mes y año inicial
        tomarPeriodos (m, a) 1 = [(m, a)]                                                          -- Caso base: si n es 1, devuelve una lista con el mes y año inicial
    
        tomarPeriodos (m, a) k = (m, a) : tomarPeriodos (siguienteMes (m, a)) (k - 1)              -- Caso recursivo: agrega el mes y año actual a la lista y luego llama a tomarPeriodos con el siguiente mes y año, usando la funcion siguienteMes


        siguienteMes :: (Int, Int) -> (Int, Int)                                                   -- Funcion auxiliar para calcular el siguiente mes y año, manejando el cambio de año
        siguienteMes (12, a) = (1, a + 1)
        siguienteMes (m,  a) = (m + 1, a)

        agregarPorCategoria :: [RegistroFinanciero] -> [(Categoria, Monto)]                        -- Funcion auxiliar para sumar los montos de los gastos por categoria, acumulando en una lista de pares (Categoria, Monto total)
        agregarPorCategoria = foldr agregar []                                                     -- Usa foldr para aplicar la funcion agregar a cada registro de gasto, acumulando los totales por categoria 
            where
                agregar reg [] = [(categoria reg, monto reg)]                                      -- Si la lista de acumulados esta vacia, agrega el primer registro como una nueva categoria con su monto
                agregar reg ((cat, total):rest)                                                    -- Si la categoria del registro coincide con la categoria actual, suma el monto al total; de lo contrario, deja el total igual y sigue buscando en el resto de la lista
                    | categoria reg == cat = (cat, total + monto reg) : rest                       -- Si la categoria del registro coincide con la categoria actual, suma el monto al total
                    | otherwise            = (cat, total) : agregar reg rest                       -- Si no coincide, deja el total igual y sigue buscando en el resto de la lista




-- ==============================================================
    -- Funcion 3: proyectarGastosMes
    -- Proyecta los gastos mensuales esperados para un mes dado,
    -- basandose en el promedio de los ultimos 3 meses anteriores.
    --
    -- Parametros:
    --   periodo: mes y año para el cual queremos la proyeccion
    --   estado:  estado actual del sistema
    --
    -- Devuelve:
    --   Lista de pares (Categoria, Monto promedio proyectado).
    --   Si no hay datos historicos, la lista puede estar vacia o con promedios parciales.
    --
    -- Nota: solo considera gastos (no ingresos).
-- ==============================================================
proyectarGastosMes :: Periodo -> EstadoSistema -> [(Categoria, Monto)]                              -- Recibe un periodo y el estado del sistema, y devuelve una lista con el gasto promedio proyectado por categoria para ese mes, basado en los 3 meses anteriores
proyectarGastosMes (Periodo mes anio) estado =
    let mesesAnteriores = tomarMesesAnteriores (mes, anio) 3                                        -- Obtiene una lista de tuplas (mes, año) para los 3 meses anteriores al periodo dado, usando la funcion tomarMesesAnteriores
        gastos = concatMap (gastosDelMes estado) mesesAnteriores                                    -- Para cada uno de esos meses, obtiene la lista de gastos por categoria usando la funcion gastosDelMes, y concatena todas esas listas en una sola lista de pares (Categoria, Monto)

        gastosOrdenados = sortBy (compare `on` fst) gastos                                          -- Ordena la lista de gastos por categoria para poder agruparlos, usando sortBy y compare con la categoria como clave        

        
        grupos = groupBy ((==) `on` fst) gastosOrdenados                                            -- Agrupa los gastos por categoria usando groupBy, comparando solo la categoria (primer elemento del par)

        
        sumaPorCat = map (\g -> (fst (head g), sum (map snd g))) grupos                             -- Para cada grupo de gastos de la misma categoria, calcula la suma total de los montos, y devuelve una lista de pares (Categoria, Monto total)
                                                                                                        --Define una funcion anonima que toma un grupo g, obtiene la categoria del primer elemento del grupo y suma los montos de todos los elementos del grupo para obtener el total por categoria
        cantidadMeses = length mesesAnteriores                                                      -- Cuenta cuantos meses se consideraron para el promedio
        promedios = [(cat, total / fromIntegral cantidadMeses) | (cat, total) <- sumaPorCat]        -- Calcula el gasto promedio proyectado por categoria dividiendo el total por la cantidad de meses considerados
    in promedios


    where
        tomarMesesAnteriores :: (Int, Int) -> Int -> [(Int, Int)]                                   -- Funcion auxiliar para generar una lista de tuplas (mes, año) para los k meses anteriores a un mes y año dado
        tomarMesesAnteriores _ 0 = []                                                               -- Caso base: si k es 0, no se consideran meses y se devuelve una lista vacia
        tomarMesesAnteriores (m, a) k =                                                             
            let ant = mesAnterior (m, a)                                                            -- Calcula el mes y año del mes anterior al dado, manejando el cambio de año, usando la funcion mesAnterior
            in ant : tomarMesesAnteriores ant (k - 1)                                               -- Caso recursivo: agrega el mes y año anterior a la lista y luego llama a tomarMesesAnteriores con ese mes y año, y k-1

        mesAnterior :: (Int, Int) -> (Int, Int)                                                     -- Funcion auxiliar para calcular el mes y año del mes anterior, manejando el cambio de año                                        
        mesAnterior (1, a) = (12, a - 1)
        mesAnterior (m, a) = (m - 1, a)

        gastosDelMes :: EstadoSistema -> (Int, Int) -> [(Categoria, Monto)]                         -- Funcion auxiliar para obtener la lista de gastos por categoria para un mes y año
        gastosDelMes est (m, a) =
            let registrosMes = filter (\r -> tipo r == Gasto && mismoMesAnio (fecha r) m a)         -- Filtra los registros del estado para obtener solo aquellos que son gastos en el mes y año dados, usando la funcion mismoMesAnio
                                      (registros est)                                                
            in map (\r -> (categoria r, monto r)) registrosMes                                      -- Transforma la lista de registros de gastos en una lista de pares (Categoria, Monto) para ese mes y año, usando map para extraer la categoria y el monto de cada registro 



-- =============================================================
-- Funcion 4: categoriasMayorGasto
    -- Determina, para un mes dado, las categorias ordenadas de mayor a menor gasto.
    --
    -- Parametros:
    --   periodo: mes y año a analizar
    --   estado:  estado actual del sistema
    --
    -- Devuelve:
    --   Lista de (Categoria, Monto total gastado en esa categoria durante el mes),
    --   ordenada descendentemente por el monto.
-- ==============================================================


categoriasMayorGasto :: Periodo -> EstadoSistema -> [(Categoria, Monto)]                    -- Recibe un periodo y el estado del sistema, y devuelve una lista de pares (Categoria, Monto total gastado en esa categoria durante el mes)
categoriasMayorGasto (Periodo mes anio) estado =                                            
    let gastosDelMes = filter (\r -> tipo r == Gasto && mismoMesAnio (fecha r) mes anio)    -- Filtra los registros del estado para obtener solo aquellos que son gastos en el mes y año dados, usando la funcion mismoMesAnio
                              (registros estado)
        sumaPorCat = foldr acumular [] gastosDelMes                                         -- Usa foldr para aplicar la funcion acumular a cada registro de gasto, acumulando los totales por categoria en una lista de pares (Categoria, Monto total)
        ordenados = sortBy (flip compare `on` snd) sumaPorCat                               -- Ordena la lista de categorias por monto total gastado en orden descendente, usando sortBy y compare con la funcion on para comparar por el segundo elemento del par (el monto) y flip para invertir el orden de comparacion
    in ordenados
  where
    acumular reg [] = [(categoria reg, monto reg)]                                          -- Si la lista de acumulados esta vacia, agrega el primer registro como una nueva categoria con su monto
    acumular reg ((cat, total):rest)
        | categoria reg == cat = (cat, total + monto reg) : rest                            -- Si la categoria del registro coincide con la categoria actual, suma el monto al total    
        | otherwise            = (cat, total) : acumular reg rest                           -- Si no coincide, deja el total igual y sigue buscando en el resto de la lista




-- =============================================================
-- Funcion 5: porcentajeGastoPorCategoria
-- ==============================================================
    -- Calcula que porcentaje del gasto total de un mes representa cada categoria.
    --
    -- Parametros:
    --   periodo: mes y año a analizar
    --   estado:  estado actual del sistema
    --
    -- Devuelve:
    --   Lista de (Categoria, Double) donde el Double es el porcentaje (0 a 100).
    --   Si no hay gastos en el mes, devuelve lista vacia.
-- ==============================================================

porcentajeGastoPorCategoria :: Periodo -> EstadoSistema -> [(Categoria, Double)]        -- Recibe un periodo y el estado del sistema, y devuelve una lista de pares (Categoria, Porcentaje)
porcentajeGastoPorCategoria periodo estado =
    let catMontos = categoriasMayorGasto periodo estado                                 -- Obtiene la lista de categorias con su monto total gastado para el mes dado, usando la funcion categoriasMayorGasto
        totalGasto = sum (map snd catMontos)                                            -- Suma el monto total gastado en todas las categorias para obtener el gasto total del mes
    in if totalGasto == 0                                                               -- Si no hay gastos en el mes, se evita la division por cero y se devuelve una lista vacia
       then []
       else map (\(cat, monto) -> (cat, (monto / totalGasto) * 100)) catMontos          -- Para cada categoria, calcula el porcentaje que representa del gasto total dividiendo el monto de la categoria por el gasto total y multiplicando por 100, y devuelve la lista de pares (Categoria, Porcentaje)
