module Main where

import Service.DonoService
import Service.AnimalService
import Service.QuartoService
import Service.ReservaService
import Tipos.Dono
import Tipos.Quarto
import Tipos.Animal (Animal(..))
import Tipos.Reserva (Reserva(..))
import Tipos.Common (TipoQuarto(..))
import Tipos.Hotel (Hotel(..), donos, animais, quartos, ocupado, reservas)

import Database.Persistence (carregarHotel, salvarHotel)
import System.IO (FilePath)

import System.IO (hFlush, stdout)
import Text.Read (readMaybe) -- Para converter String para Int/Float

dbArquivo :: FilePath
dbArquivo = "hotel.json"

-- inicializacao
main :: IO ()
main = do

    hotelInicial <- carregarHotel dbArquivo
    putStrLn $ "Bem-vindo ao " ++ nomeHotel hotelInicial ++ "!"

    appLoop hotelInicial

-- Loop Principal da Aplicação

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

listarTudo :: Hotel -> IO ()
listarTudo hotel = do
    handleListarDonos hotel
    handleListarAnimais hotel
    handleListarQuartos hotel
    handleListarReservas hotel


-- ADICIONAR DONO
handleAdicionarDono :: Hotel -> IO (Hotel, Either String Dono)
handleAdicionarDono hotel = do
    putStrLn "--- Adicionar Novo Dono ---"
    cpf <- prompt "CPF:"
    nome <- prompt "Nome:"
    tel <- prompt "Telefone:"
    email <- prompt "Email:"

    let novoDono = Dono nome tel email cpf
    return $ adicionarDono novoDono hotel

-- ATUALIZAR DONO
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

-- REMOVER DONO
handleRemoverDono :: Hotel -> IO (Hotel, Either String ())
handleRemoverDono hotel = do
    putStrLn "--- Remover Dono ---"
    cpf <- prompt "CPF do dono a remover:"
    return $ removerDono cpf hotel

-- ADICIONAR ANIMAL
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

-- ADICIONAR QUARTO
handleAdicionarQuarto :: Hotel -> IO (Hotel, Either String Quarto)
handleAdicionarQuarto hotel = do
    putStrLn "--- Adicionar Novo Quarto ---"
    numStr <- prompt "Número do Quarto (ex: 101):"
    tipoStr <- prompt "Tipo (1=Simples, 2=Luxo, 3=VIP):"

    let tipoQuarto = case tipoStr of
                        "2" -> Luxo
                        "3" -> VIP
                        _   -> Simples -- Padrão

    case readMaybe numStr of
        Nothing -> return (hotel, Left "Número do quarto inválido.")
        Just numQuarto ->
            let novoQuarto = Quarto numQuarto tipoQuarto False
            in return $ adicionarQuarto novoQuarto hotel

-- ADICIONAR RESERVA
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

-- REMOVER RESERVA
handleRemoverReserva :: Hotel -> IO (Hotel, Either String ())
handleRemoverReserva hotel = do
    putStrLn "--- Finalizar Reserva (Check-out) ---"
    reservaIDStr <- prompt "ID da Reserva a finalizar:"

    case readMaybe reservaIDStr of
        Nothing -> return (hotel, Left "ID da reserva inválido.")
        Just reservaID ->
            return $ removerReserva reservaID hotel

handleListarDonos :: Hotel -> IO ()
handleListarDonos hotel = do
    putStrLn "\n--- Lista de Donos ---"
    let lista = donos hotel
    if null lista
    then putStrLn "Nenhum dono cadastrado."
    else mapM_ (putStrLn . formatDono) lista
    where
        formatDono d = "  - CPF: " ++ cpfDono d ++ ", Nome: " ++ nomeDono d ++ ", Tel: " ++ telefoneDono d

handleListarAnimais :: Hotel -> IO ()
handleListarAnimais hotel = do
    putStrLn "\n--- Lista de Animais ---"
    let lista = animais hotel
    if null lista
    then putStrLn "Nenhum animal cadastrado."
    else mapM_ (putStrLn . formatAnimal) lista
    where
        formatAnimal a = "  - ID: " ++ show (animalID a) ++ ", Nome: " ++ nomeAnimal a ++ ", Espécie: " ++ especieAnimal a ++ ", Dono (CPF): " ++ donoCpfAnimal a

handleListarQuartos :: Hotel -> IO ()
handleListarQuartos hotel = do
    putStrLn "\n--- Lista de Quartos ---"
    let lista = quartos hotel
    if null lista
    then putStrLn "Nenhum quarto cadastrado."
    else mapM_ (putStrLn . formatQuarto) lista
    where
        formatQuarto q = "  - Quarto: " ++ show (numeroQuarto q) ++ ", Tipo: " ++ show (tipoQuarto q) ++ ", Ocupado: " ++ show (ocupado q)

handleListarReservas :: Hotel -> IO ()
handleListarReservas hotel = do
    putStrLn "\n--- Lista de Reservas Ativas ---"
    let lista = reservas hotel
    if null lista
    then putStrLn "Nenhuma reserva ativa."
    else mapM_ (putStrLn . formatReserva) lista
    where
        formatReserva r = "  - Reserva ID: " ++ show (reservaID r) ++ ", Animal ID: " ++ show (animalIDReserva r) ++ ", Quarto: " ++ show (quartoIDReserva r) ++ ", Saída: " ++ dataSaida r


prompt :: String -> IO String
prompt texto = do
    putStr (texto ++ " ")
    hFlush stdout
    getLine