-- Guarda só o prompt.
module Interface.Utils (prompt) where

import System.IO (hFlush, stdout)

-- Essa é uma versão melhorada do 'getLine'.
-- a diferença é que o 'hFlush stdout' força o
-- "Escolha:" a aparecer na tela antes do usuário digitar.
-- o 'getLine' normal às vezes espera o 'Enter' pra mostrar o prompt.
prompt :: String -> IO String
prompt mensagem = do
    putStr (mensagem ++ " ")
    hFlush stdout
    getLine