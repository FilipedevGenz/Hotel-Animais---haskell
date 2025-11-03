-- menu principal.
module Interface.CLI (iniciarInterface) where

-- pra poder carregar e salvar o hotel.json
import Database.Persistence (carregarHotel, salvarHotel)
import System.IO (FilePath)
import Tipos.Hotel (Hotel(..)) 
import Interface.Utils (prompt) 
-- Puxa os "sub-menus" de cada entidade
import Interface.DonoCLI (gerenciarDonos, handleListarDonos)
import Interface.AnimalCLI (gerenciarAnimais, handleListarAnimais)
import Interface.QuartoCLI (gerenciarQuartos, handleListarQuartos)
import Interface.ReservaCLI (gerenciarReservas, handleListarReservas)

-- o nome do "banco de dados" json
dbArquivo :: FilePath
dbArquivo = "hotel.json"

-- função que o Main.hs chama.
-- ela carrega o hotel do JSON  e começa o loop principal
iniciarInterface :: IO ()
iniciarInterface = do
    hotelInicial <- carregarHotel dbArquivo
    putStrLn $ "Bem-vindo ao " ++ nomeHotel hotelInicial ++ "!"
    appLoop hotelInicial 

-- o loop principal do programa, que é infinito até o usuário escolher sair
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

    -- o "roteador" principal do app
    case opcao of
        -- 1. 'gerenciarDonos hotel' roda. Ele é um 'IO Hotel', ou seja,
        --    ele vai devolver o hotel ATUALIZADO (depois de adicionar/remover/etc).
        -- 2. O '>>=' (bind) "pega" esse 'novoHotel' que saiu do 'gerenciarDonos'.
        -- 3. 'appLoop' (a nossa recursão) é chamado com esse 'novoHotel'.
        -- isso garante que o estado do hotel tá sempre atualizado.
        "1" -> gerenciarDonos hotel >>= appLoop
        "2" -> gerenciarAnimais hotel >>= appLoop
        "3" -> gerenciarQuartos hotel >>= appLoop
        "4" -> gerenciarReservas hotel >>= appLoop
        "5" -> listarTudo hotel >> appLoop hotel
        "0" -> do
            salvarHotel dbArquivo hotel
            putStrLn "Dados salvos. Até mais!"
            
        -- se digitar qualquer outra coisa, só roda o loop de novo
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            appLoop hotel

-- ela chama as funções de listar de todos os outros módulos
listarTudo :: Hotel -> IO ()
listarTudo hotel = do
    handleListarDonos hotel
    handleListarAnimais hotel
    handleListarQuartos hotel
    handleListarReservas hotel