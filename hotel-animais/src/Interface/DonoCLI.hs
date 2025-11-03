module Interface.DonoCLI (gerenciarDonos, handleListarDonos) where

import Tipos.Hotel (Hotel(..), donos)
import Tipos.Dono
import Service.DonoService
import Interface.Utils (prompt) -- Importa o prompt do novo módulo

-- Função principal do módulo.
-- Note que ela agora retorna 'IO Hotel' (o estado atualizado do hotel)
gerenciarDonos :: Hotel -> IO Hotel
gerenciarDonos hotel = do
    putStrLn "\n=== Gerenciar Donos ==="
    putStrLn "1. Adicionar Dono"
    putStrLn "2. Listar Donos"
    putStrLn "3. Atualizar Dono"
    putStrLn "4. Remover Dono"
    putStrLn "0. Voltar"
    putStrLn "======================="
    
    opcao <- prompt "Escolha:"
    
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarDono hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right dono -> putStrLn $ "\nSucesso! Dono '" ++ nomeDono dono ++ "' adicionado."
            gerenciarDonos novoHotel -- Chama a si mesmo recursivamente
            
        "2" -> handleListarDonos hotel >> gerenciarDonos hotel
        
        "3" -> do
            (novoHotel, res) <- handleAtualizarDono hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right dono -> putStrLn $ "\nSucesso! Dono '" ++ nomeDono dono ++ "' atualizado."
            gerenciarDonos novoHotel

        "4" -> do
            (novoHotel, res) <- handleRemoverDono hotel
            case res of
                Left err -> putStrLn $ "\nERRO: " ++ err
                Right _  -> putStrLn "\nSucesso! Dono removido."
            gerenciarDonos novoHotel
            
        "0" -> return hotel -- Retorna o estado atualizado do hotel para o appLoop
        _   -> putStrLn "Opção inválida." >> gerenciarDonos hotel


handleAdicionarDono :: Hotel -> IO (Hotel, Either String Dono)
handleAdicionarDono hotel = do
    putStrLn "--- Adicionar Novo Dono ---"
    cpf <- prompt "CPF:"
    nome <- prompt "Nome:"
    tel <- prompt "Telefone:"
    email <- prompt "Email:"
    
    let novoDono = Dono nome tel email cpf
    return $ adicionarDono novoDono hotel

handleAtualizarDono :: Hotel -> IO (Hotel, Either String Dono)
handleAtualizarDono hotel = do
    putStrLn "--- Atualizar Dono ---"
    cpf <- prompt "CPF do dono a atualizar:"
    
    case buscarDono cpf hotel of
        Nothing -> return (hotel, Left "Dono não encontrado.")
        Just donoAntigo -> do
            putStrLn $ "Dono encontrado: " ++ nomeDono donoAntigo
            novoNome <- prompt $ "Novo Nome (Atual: " ++ nomeDono donoAntigo ++ "):"
            novoTel <- prompt $ "Novo Telefone (Atual: " ++ telefoneDono donoAntigo ++ "):"
            novoEmail <- prompt $ "Novo Email (Atual: " ++ emailDono donoAntigo ++ "):"
            
            let fnAtualiza d = d { nomeDono = novoNome, telefoneDono = novoTel, emailDono = novoEmail }
            return $ atualizarDono cpf fnAtualiza hotel

handleRemoverDono :: Hotel -> IO (Hotel, Either String ())
handleRemoverDono hotel = do
    putStrLn "--- Remover Dono ---"
    cpf <- prompt "CPF do dono a remover:"
    return $ removerDono cpf hotel

-- Funções de Listagem (movidas para cá)
imprimirDono :: Dono -> IO ()
imprimirDono dono = do
    putStrLn $ "  Nome:     " ++ nomeDono dono
    putStrLn $ "  CPF:      " ++ cpfDono dono
    putStrLn $ "  Telefone: " ++ telefoneDono dono
    putStrLn $ "  Email:    " ++ emailDono dono
    putStrLn "  --------------------------------" 

handleListarDonos :: Hotel -> IO ()
handleListarDonos hotel = do
    putStrLn "\n--- LISTA DE DONOS ---"
    let lista = donos hotel
    if null lista
    then putStrLn "Nenhum dono cadastrado."
    else mapM_ imprimirDono lista