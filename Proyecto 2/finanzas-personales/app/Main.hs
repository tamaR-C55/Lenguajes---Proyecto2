module Main where


import Types
import Presupuesto

main :: IO ()
main = do
    let estadoInicial = estadoVacio

    estadoFinal <- definirPresupuestoIO estadoInicial

    print estadoFinal