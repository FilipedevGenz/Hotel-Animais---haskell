module Interface.ReservaCLI (gerenciarReservas, handleListarReservas) where

-- Imports de Tipos
import Tipos.Hotel (Hotel(..), reservas)
import Tipos.Reserva
import Tipos.Common (Data) -- Importa o tipo 'Data' de Common

-- Imports de Serviços
import Service.ReservaService

-- Imports de Utilitários
import Interface.Utils (prompt)
import Text.Read (readMaybe)

-- Loop principal da gerência de reservas
-- Retorna o estado atualizado do hotel
gerenciarReservas :: Hotel -> IO Hotel
gerenciarReservas hotel = do
    putStrLn "\n=== Gerenciar Reservas ==="
    putStrLn "1. Criar Reserva (Check-in)"
    putStrLn "2. Finalizar Reserva (Check-out)"
    putStrLn "3. Listar Reservas Ativas"
    putStrLn "0. Voltar"
    putStrLn "========================="

    opcao <- prompt "Escolha:"
    
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarReserva hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right reserva -> putStrLn $ "\nSucesso! Reserva " ++ show (reservaID reserva) ++ " criada."
            gerenciarReservas novoHotel -- Chama a si mesmo
            
        "2" -> do
            (novoHotel, res) <- handleRemoverReserva hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right _   -> putStrLn $ "\nSucesso! Check-out realizado."
            gerenciarReservas novoHotel -- Chama a si mesmo
            
        "3" -> handleListarReservas hotel >> gerenciarReservas hotel
        "0" -> return hotel -- Retorna o estado atualizado para o loop principal
        _   -> putStrLn "Opção inválida." >> gerenciarReservas hotel

-- Handlers
handleAdicionarReserva :: Hotel -> IO (Hotel, Either String Reserva)
handleAdicionarReserva hotel = do
    putStrLn "--- Criar Nova Reserva (Check-in) ---"
    animalIDStr <- prompt "ID do Animal:"
    quartoIDStr <- prompt "Número do Quarto:"
    dtEntrada <- prompt "Data de Entrada (dd/mm/aaaa):"
    dtSaida <- prompt "Data de Saída (dd/mm/aaaa):"
    precoStr <- prompt "Preço Total (ex: 250.0):"

    case (readMaybe animalIDStr, readMaybe quartoIDStr, readMaybe precoStr) of
        (Nothing, _, _) -> return (hotel, Left "ID do animal inválido.")
        (_, Nothing, _) -> return (hotel, Left "Número do quarto inválido.")
        (_, _, Nothing) -> return (hotel, Left "Preço inválido.")
        (Just animalID, Just quartoID, Just preco) ->
            return $ adicionarReserva animalID quartoID dtEntrada dtSaida preco hotel

handleRemoverReserva :: Hotel -> IO (Hotel, Either String ())
handleRemoverReserva hotel = do
    putStrLn "--- Finalizar Reserva (Check-out) ---"
    reservaIDStr <- prompt "ID da Reserva a finalizar:"
    
    case readMaybe reservaIDStr of
        Nothing -> return (hotel, Left "ID da reserva inválido.")
        Just reservaID ->
            return $ removerReserva reservaID hotel

-- Funções de Listagem
imprimirReserva :: Reserva -> IO ()
imprimirReserva reserva = do
    putStrLn $ "  ID Reserva: " ++ show (reservaID reserva)
    putStrLn $ "  ID Animal:  " ++ show (animalIDReserva reserva)
    putStrLn $ "  ID Quarto:  " ++ show (quartoIDReserva reserva)
    putStrLn $ "  Entrada:    " ++ dataEntrada reserva
    putStrLn $ "  Saída:      " ++ dataSaida reserva
    putStrLn $ "  Preço:      R$ " ++ show (precoTotal reserva)
    putStrLn "  --------------------------------" -- Separador

handleListarReservas :: Hotel -> IO ()
handleListarReservas hotel = do
    putStrLn "\n--- LISTA DE RESERVAS ---"
    let lista = reservas hotel
    if null lista
    then putStrLn "Nenhuma reserva cadastrada."
    else mapM_ imprimirReserva lista