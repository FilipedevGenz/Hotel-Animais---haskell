-- menu Donos
module Interface.DonoCLI (gerenciarDonos, handleListarDonos) where

import Tipos.Hotel (Hotel(..), donos) 
import Tipos.Dono
import Service.DonoService 
import Interface.Utils (prompt) 

-- o loop principal de donos 
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
    
    -- swich do menu
    case opcao of
        "1" -> do
            -- chama o 'handle' pra pegar os dados
            (novoHotel, res) <- handleAdicionarDono hotel
            -- trata a resposta do service (erro ou sucesso)
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right dono -> putStrLn $ "\nSucesso! Dono '" ++ nomeDono dono ++ "' adicionado."
            gerenciarDonos novoHotel -- chama a si mesmo com o novo estado 
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
        "0" -> return hotel 
        _   -> putStrLn "Opção inválida." >> gerenciarDonos hotel

-- o formulário pra adicionar um dono
handleAdicionarDono :: Hotel -> IO (Hotel, Either String Dono)
handleAdicionarDono hotel = do
    putStrLn "--- Adicionar Novo Dono ---"
    cpf <- prompt "CPF:"
    nome <- prompt "Nome:"
    tel <- prompt "Telefone:"
    email <- prompt "Email:"
    
    let novoDono = Dono nome tel email cpf
    -- passa o Dono pro 'Service', que vai fazer a validação do cpf
    return $ adicionarDono novoDono hotel

-- formulario pra atualizar um dono
handleAtualizarDono :: Hotel -> IO (Hotel, Either String Dono)
handleAtualizarDono hotel = do
    putStrLn "--- Atualizar Dono ---"
    cpf <- prompt "CPF do dono a atualizar:"
    
    -- verifica se o dono existe
    case buscarDono cpf hotel of
        Nothing -> return (hotel, Left "Dono não encontrado.")
    
        Just donoAntigo -> do
            putStrLn $ "Dono encontrado: " ++ nomeDono donoAntigo
            novoNome <- prompt $ "Novo Nome (Atual: " ++ nomeDono donoAntigo ++ "):"
            novoTel <- prompt $ "Novo Telefone (Atual: " ++ telefoneDono donoAntigo ++ "):"
            novoEmail <- prompt $ "Novo Email (Atual: " ++ emailDono donoAntigo ++ "):"
            
            -- a gente cria uma função 'fnAtualiza' que sabe como aplicar as mudanças
            let fnAtualiza d = d { nomeDono = novoNome, telefoneDono = novoTel, emailDono = novoEmail }
            -- e passa essa função pro 'Service'. O Service que vai aplicar ela
            return $ atualizarDono cpf fnAtualiza hotel

-- o formulário pra remover. bem simples.
handleRemoverDono :: Hotel -> IO (Hotel, Either String ())
handleRemoverDono hotel = do
    putStrLn "--- Remover Dono ---"
    cpf <- prompt "CPF do dono a remover:"
    -- só passa o CPF pro 'Service'. Ele que vai checar as regras
    return $ removerDono cpf hotel

imprimirDono :: Dono -> IO ()
imprimirDono dono = do
    putStrLn $ "  Nome:     " ++ nomeDono dono
    putStrLn $ "  CPF:      " ++ cpfDono dono
    putStrLn $ "  Telefone: " ++ telefoneDono dono
    putStrLn $ "  Email:    " ++ emailDono dono
    putStrLn "  --------------------------------" 

-- lista todos os donos
handleListarDonos :: Hotel -> IO ()
handleListarDonos hotel = do
    putStrLn "\n--- LISTA DE DONOS ---"
    let lista = donos hotel
    if null lista
    then putStrLn "Nenhum dono cadastrado."
    else mapM_ imprimirDono lista