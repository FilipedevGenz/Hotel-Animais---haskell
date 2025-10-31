module Database.Persistence
    ( carregarHotel
    , salvarHotel
    ) where

import Tipos.Hotel (Hotel)
import Database.State (initialHotel)
import Data.Aeson (FromJSON, ToJSON, decode, encodePretty)
import qualified Data.ByteString.Lazy as B
import System.Directory (doesFileExist)
import System.IO (FilePath)

-- Salva em um arquivo JSON.
salvarHotel :: FilePath -> Hotel -> IO ()
salvarHotel arquivo hotel = do
    putStrLn $ "Salvando dados em " ++ arquivo ++ "..."
    B.writeFile arquivo (encodePretty hotel)
    putStrLn "Dados salvos com sucesso."

-- Tenta carregar o estado do hotel de um arquivo JSON.
-- Se o arquivo não existir ou estiver corrompido, retorna o 'initialHotel'.
carregarHotel :: FilePath -> IO Hotel
carregarHotel arquivo = do
    putStrLn $ "Carregando dados de " ++ arquivo ++ "..."
    existe <- doesFileExist arquivo

    if not existe
    then do
        putStrLn "Arquivo não encontrado. Criando novo hotel..."
        return initialHotel
    else do
        conteudo <- B.readFile arquivo
        case decode conteudo of
            Nothing -> do
                putStrLn "Erro ao ler o arquivo (corrompido?). Iniciando com hotel vazio."
                return initialHotel
            Just hotel -> do
                putStrLn "Dados carregados com sucesso."
                return hotel