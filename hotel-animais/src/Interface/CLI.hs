module Interface.CLI (iniciarInterface) where

-- Imports de Sistema
import Database.Persistence (carregarHotel, salvarHotel)
import System.IO (FilePath)

-- Imports de Tipos
import Tipos.Hotel (Hotel(..))

-- Imports da própria Interface (os novos módulos)
import Interface.Utils (prompt)
import Interface.DonoCLI (gerenciarDonos, handleListarDonos)
import Interface.AnimalCLI (gerenciarAnimais, handleListarAnimais)
import Interface.QuartoCLI (gerenciarQuartos, handleListarQuartos)     -- (Você precisa criar este arquivo)
import Interface.ReservaCLI (gerenciarReservas, handleListarReservas) -- (Você precisa criar este arquivo)

dbArquivo :: FilePath
dbArquivo = "hotel.json"

iniciarInterface :: IO ()
iniciarInterface = do
    hotelInicial <- carregarHotel dbArquivo
    putStrLn $ "Bem-vindo ao " ++ nomeHotel hotelInicial ++ "!"
    appLoop hotelInicial

appLoop :: Hotel -> IO ()
appLoop hotel = do
    putStrLn "\n=== MENU PRINCIPAL ==="
    putStrLn "1. Gerenciar Donos"
    putStrLn "2. Gerenciar Animais"
    putStrLn "3. Gerenciar Quartos"
    putStrLn "4. Gerenciar Reservas"
    putStrLn "5. Listar Tudo"
    putStrLn "0. Salvar e Sair"
    putStrLn "======================"
    
    opcao <- prompt "Escolha uma opção:"

    -- A MUDANÇA ESTÁ AQUI:
    case opcao of
        -- Usamos '>>=' (bind) para pegar o 'novoHotel' que o
        -- 'gerenciarDonos' retorna e passá-lo para a próxima chamada do 'appLoop'
        "1" -> gerenciarDonos hotel >>= appLoop
        "2" -> gerenciarAnimais hotel >>= appLoop
        "3" -> gerenciarQuartos hotel >>= appLoop
        "4" -> gerenciarReservas hotel >>= appLoop
        "5" -> listarTudo hotel >> appLoop hotel
        "0" -> do
            salvarHotel dbArquivo hotel
            putStrLn "Dados salvos. Até mais!"
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            appLoop hotel

-- Esta função agora depende dos 'handleListar...'
-- que são importados dos outros módulos
listarTudo :: Hotel -> IO ()
listarTudo hotel = do
    handleListarDonos hotel
    handleListarAnimais hotel
    handleListarQuartos hotel
    handleListarReservas hotel