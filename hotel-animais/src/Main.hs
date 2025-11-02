module Main where

import Service.DonoService
import Service.AnimalService
import Service.QuartoService
import Service.ReservaService
import Tipos.Dono
import Tipos.Quarto (Quarto(..), TipoQuarto(..), ocupado)
import Tipos.Animal (Animal(..))
import Tipos.Reserva (Reserva(..))
import Tipos.Common 
import Tipos.Hotel (Hotel(..), donos, animais, quartos, reservas)
import Database.Persistence (carregarHotel, salvarHotel)
import System.IO (FilePath, hFlush, stdout)
import Text.Read (readMaybe) 

dbArquivo :: FilePath
dbArquivo = "hotel.json"

main :: IO ()
main = do
    hotelInicial <- carregarHotel dbArquivo
    putStrLn $ "Bem-vindo ao " ++ nomeHotel hotelInicial ++ "!"
    appLoop hotelInicial

appLoop :: Hotel -> IO ()
appLoop hotel = do
    putStrLn "\n--- Menu Principal ---"
    putStrLn "1. Gerenciar Donos"
    putStrLn "2. Gerenciar Animais"
    putStrLn "3. Gerenciar Quartos"
    putStrLn "4. Gerenciar Reservas"
    putStrLn "5. Listar Tudo"
    putStrLn "0. Salvar e Sair"
    
    opcao <- prompt "Escolha uma opção:"

    case opcao of
        "1" -> gerenciarDonos hotel
        "2" -> gerenciarAnimais hotel
        "3" -> gerenciarQuartos hotel
        "4" -> gerenciarReservas hotel
        "5" -> listarTudo hotel >> appLoop hotel
        "0" -> do
            salvarHotel dbArquivo hotel
            putStrLn "Dados salvos. Até mais!"
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            appLoop hotel

-------------------------------------
-- Seção: Donos
-------------------------------------

gerenciarDonos :: Hotel -> IO ()
gerenciarDonos hotel = do
    putStrLn "\n--- Gerenciar Donos ---"
    putStrLn "1. Adicionar Dono"
    putStrLn "2. Listar Donos"
    putStrLn "3. Atualizar Dono"
    putStrLn "4. Remover Dono"
    putStrLn "0. Voltar"
    
    opcao <- prompt "Escolha:"
    
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarDono hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right dono -> putStrLn $ "\nSucesso! Dono '" ++ nomeDono dono ++ "' adicionado."
            appLoop novoHotel
            
        "2" -> handleListarDonos hotel >> appLoop hotel
        
        "3" -> do
            (novoHotel, res) <- handleAtualizarDono hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right dono -> putStrLn $ "\nSucesso! Dono '" ++ nomeDono dono ++ "' atualizado."
            appLoop novoHotel

        "4" -> do
            (novoHotel, res) <- handleRemoverDono hotel
            case res of
                Left err -> putStrLn $ "\nERRO: " ++ err
                Right _  -> putStrLn "\nSucesso! Dono removido."
            appLoop novoHotel
            
        "0" -> appLoop hotel
        _   -> putStrLn "Opção inválida." >> appLoop hotel


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

-------------------------------------
-- Seção: Animais
-------------------------------------

gerenciarAnimais :: Hotel -> IO ()
gerenciarAnimais hotel = do
    putStrLn "\n--- Gerenciar Animais ---"
    putStrLn "1. Adicionar Animal"
    putStrLn "2. Listar Animais"
    putStrLn "0. Voltar"

    opcao <- prompt "Escolha:"
    
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarAnimal hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right animal -> putStrLn $ "\nSucesso! Animal '" ++ nomeAnimal animal ++ "' adicionado."
            appLoop novoHotel

        "2" -> handleListarAnimais hotel >> appLoop hotel
        "0" -> appLoop hotel
        _   -> putStrLn "Opção inválida." >> appLoop hotel

handleAdicionarAnimal :: Hotel -> IO (Hotel, Either String Animal)
handleAdicionarAnimal hotel = do
    putStrLn "--- Adicionar Novo Animal ---"
    cpfDono <- prompt "CPF do Dono:"
    nome <- prompt "Nome do Animal:"
    idadeStr <- prompt "Idade:"
    especie <- prompt "Espécie (ex: Cachorro):"
    raca <- prompt "Raça (ex: Poodle):"
    pesoStr <- prompt "Peso (ex: 8.5):"
    
    case (readMaybe idadeStr, readMaybe pesoStr) of
        (Nothing, _) -> return (hotel, Left "Idade inválida.")
        (_, Nothing) -> return (hotel, Left "Peso inválido.")
        (Just idade, Just peso) ->
            return $ adicionarAnimal cpfDono nome idade especie raca peso hotel

-------------------------------------
-- Seção: Quartos
-------------------------------------

gerenciarQuartos :: Hotel -> IO ()
gerenciarQuartos hotel = do
    putStrLn "\n--- Gerenciar Quartos ---"
    putStrLn "1. Adicionar Quarto"
    putStrLn "2. Listar Quartos"
    putStrLn "0. Voltar"

    opcao <- prompt "Escolha:"
    
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarQuarto hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right quarto -> putStrLn $ "\nSucesso! Quarto " ++ show (numeroQuarto quarto) ++ " adicionado."
            appLoop novoHotel
            
        "2" -> handleListarQuartos hotel >> appLoop hotel
        "0" -> appLoop hotel
        _   -> putStrLn "Opção inválida." >> appLoop hotel

handleAdicionarQuarto :: Hotel -> IO (Hotel, Either String Quarto)
handleAdicionarQuarto hotel = do
    putStrLn "--- Adicionar Novo Quarto ---"
    numStr <- prompt "Número do Quarto (ex: 101):"
    tipoStr <- prompt "Tipo (1=Simples, 2=Luxo, 3=VIP):"
    
    let tipoQuarto = case tipoStr of
                        "2" -> Luxo
                        "3" -> VIP
                        _   -> Simples

    case readMaybe numStr of
        Nothing -> return (hotel, Left "Número do quarto inválido.")
        Just numQuarto ->
            let novoQuarto = Quarto numQuarto tipoQuarto False
            in return $ adicionarQuarto novoQuarto hotel

-------------------------------------
-- Seção: Reservas
-------------------------------------

gerenciarReservas :: Hotel -> IO ()
gerenciarReservas hotel = do
    putStrLn "\n--- Gerenciar Reservas ---"
    putStrLn "1. Criar Reserva (Check-in)"
    putStrLn "2. Finalizar Reserva (Check-out)"
    putStrLn "3. Listar Reservas Ativas"
    putStrLn "0. Voltar"

    opcao <- prompt "Escolha:"
    
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarReserva hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right reserva -> putStrLn $ "\nSucesso! Reserva " ++ show (reservaID reserva) ++ " criada."
            appLoop novoHotel
            
        "2" -> do
            (novoHotel, res) <- handleRemoverReserva hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right _   -> putStrLn $ "\nSucesso! Check-out realizado."
            appLoop novoHotel
            
        "3" -> handleListarReservas hotel >> appLoop hotel
        "0" -> appLoop hotel
        _   -> putStrLn "Opção inválida." >> appLoop hotel

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

-------------------------------------
-- Utilitários
-------------------------------------

listarTudo :: Hotel -> IO ()
listarTudo hotel = do
    handleListarDonos hotel
    handleListarAnimais hotel
    handleListarQuartos hotel
    handleListarReservas hotel

-------------------------------------
-- Funções auxiliares de listagem
-------------------------------------

handleListarDonos :: Hotel -> IO ()
handleListarDonos hotel = do
    putStrLn "\n--- Lista de Donos ---"
    mapM_ print (donos hotel)

handleListarAnimais :: Hotel -> IO ()
handleListarAnimais hotel = do
    putStrLn "\n--- Lista de Animais ---"
    mapM_ print (animais hotel)

handleListarQuartos :: Hotel -> IO ()
handleListarQuartos hotel = do
    putStrLn "\n--- Lista de Quartos ---"
    mapM_ print (quartos hotel)

handleListarReservas :: Hotel -> IO ()
handleListarReservas hotel = do
    putStrLn "\n--- Lista de Reservas ---"
    mapM_ print (reservas hotel)


prompt :: String -> IO String
prompt mensagem = do
    putStr (mensagem ++ " ")
    hFlush stdout
    getLine

