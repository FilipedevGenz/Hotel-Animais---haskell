module Interface.Utils (prompt) where

import System.IO (hFlush, stdout)

-- A função 'prompt' que antes estava no Main.hs
prompt :: String -> IO String
prompt mensagem = do
    putStr (mensagem ++ " ")
    hFlush stdout
    getLine