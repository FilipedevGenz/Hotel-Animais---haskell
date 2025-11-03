-- src/Main.hs

module Main where

-- 1. Importa apenas a UMA função que inicia a interface
import Interface.CLI (iniciarInterface)

-- 2. A função main agora apenas chama essa função
main :: IO ()
main = iniciarInterface