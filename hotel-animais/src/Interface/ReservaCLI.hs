-- Menu de "Gerenciar Reservas" (check-in / check-out)
module Interface.ReservaCLI (gerenciarReservas, handleListarReservas) where

import Tipos.Hotel (Hotel(..), reservas)
import Tipos.Reserva
import Tipos.Common (Data) 
import Service.ReservaService -- puxa a lógica (adicionarReserva, etc)
import Interface.Utils (prompt)
import Text.Read (readMaybe) -- pra validar IDs e preços

-- Loop principal da gerência de reservas
-- que devolve o hotel atualizado
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
        "1" -> do -- Check-in
            (novoHotel, res) <- handleAdicionarReserva hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right reserva -> putStrLn $ "\nSucesso! Reserva " ++ show (reservaID reserva) ++ " criada."
            gerenciarReservas novoHotel -- recursão
            
        "2" -> do -- Check-out
            (novoHotel, res) <- handleRemoverReserva hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right _   -> putStrLn $ "\nSucesso! Check-out realizado."
            gerenciarReservas novoHotel -- recursão
            
        "3" -> handleListarReservas hotel >> gerenciarReservas hotel
        "0" -> return hotel -- Ponto de saída!
        _   -> putStrLn "Opção inválida." >> gerenciarReservas hotel

-- "Formulário" de Check-in
handleAdicionarReserva :: Hotel -> IO (Hotel, Either String Reserva)
handleAdicionarReserva hotel = do
    putStrLn "--- Criar Nova Reserva (Check-in) ---"
    animalIDStr <- prompt "ID do Animal:"
    quartoIDStr <- prompt "Número do Quarto:"
    dtEntrada <- prompt "Data de Entrada (dd/mm/aaaa):"
    dtSaida <- prompt "Data de Saída (dd/mm/aaaa):"
    precoStr <- prompt "Preço Total (ex: 250.0):"

    -- validação tripla com 'readMaybe' pra garantir que os IDs/Preço são números
    case (readMaybe animalIDStr, readMaybe quartoIDStr, readMaybe precoStr) of
        (Nothing, _, _) -> return (hotel, Left "ID do animal inválido.")
        (_, Nothing, _) -> return (hotel, Left "Número do quarto inválido.")
        (_, _, Nothing) -> return (hotel, Left "Preço inválido.")
        (Just animalID, Just quartoID, Just preco) ->
            -- se tudo for número, passa pro 'Service'.
            -- o Service vai validar as regras (quarto ocupado, data, etc).
            return $ adicionarReserva animalID quartoID dtEntrada dtSaida preco hotel

-- "Formulário" de Check-out
handleRemoverReserva :: Hotel -> IO (Hotel, Either String ())
handleRemoverReserva hotel = do
    putStrLn "--- Finalizar Reserva (Check-out) ---"
    reservaIDStr <- prompt "ID da Reserva a finalizar:"
    
    -- valida se o ID digitado é um número
    case readMaybe reservaIDStr of
        Nothing -> return (hotel, Left "ID da reserva inválido.")
        Just reservaID ->
            -- passa pro 'Service' fazer a remoção (e liberar o quarto)
            return $ removerReserva reservaID hotel

-- como imprimir uma reserva na tela.
imprimirReserva :: Reserva -> IO ()
imprimirReserva reserva = do 
    putStrLn $ "  ID Reserva: " ++ show (reservaID reserva)
    putStrLn $ "  ID Animal:  " ++ show (animalIDReserva reserva)
    putStrLn $ "  ID Quarto:  " ++ show (quartoIDReserva reserva)
    putStrLn $ "  Entrada:    " ++ dataEntrada reserva
    putStrLn $ "  Saída:      " ++ dataSaida reserva
    putStrLn $ "  Preço:      R$ " ++ show (precoTotal reserva)
    putStrLn " "--------------------------------" 

-- lista todas as reservas.
handleListarReservas :: Hotel -> IO ()
handleListarReservas hotel = do
    putStrLn "\n--- LISTA DE RESERVAS ---"
    let lista = reservas hotel
    if null lista
    then putStrLn "Nenhuma reserva cadastrada."
    else mapM_ imprimirReserva lista