module Interface.AnimalCLI (gerenciarAnimais, handleListarAnimais) where

import Tipos.Hotel (Hotel(..), animais)
import Tipos.Animal
import Tipos.Common
import Service.AnimalService
import Interface.Utils (prompt)
import Text.Read (readMaybe)

gerenciarAnimais :: Hotel -> IO Hotel
gerenciarAnimais hotel = do
    putStrLn "\n=== Gerenciar Animais ==="
    putStrLn "1. Adicionar Animal"
    putStrLn "2. Listar Animais"
    putStrLn "0. Voltar"
    putStrLn "========================="

    opcao <- prompt "Escolha:"
    
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarAnimal hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right animal -> putStrLn $ "\nSucesso! Animal '" ++ nomeAnimal animal ++ "' adicionado."
            gerenciarAnimais novoHotel

        "2" -> handleListarAnimais hotel >> gerenciarAnimais hotel
        "0" -> return hotel
        _   -> putStrLn "Opção inválida." >> gerenciarAnimais hotel

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

-- Funções de Listagem
imprimirAnimal :: Animal -> IO ()
imprimirAnimal animal = do
    putStrLn $ "  ID:       " ++ show (animalID animal)
    putStrLn $ "  Nome:     " ++ nomeAnimal animal
    putStrLn $ "  Idade:    " ++ show (idadeAnimal animal)
    putStrLn $ "  Espécie:  " ++ especieAnimal animal
    putStrLn $ "  Raça:     " ++ racaAnimal animal
    putStrLn $ "  Peso:     " ++ show (pesoAnimal animal) ++ " kg"
    putStrLn $ "  CPF Dono: " ++ donoCpfAnimal animal
    putStrLn "  --------------------------------"

handleListarAnimais :: Hotel -> IO ()
handleListarAnimais hotel = do
    putStrLn "\n--- LISTA DE ANIMAIS ---"
    let lista = animais hotel
    if null lista
    then putStrLn "Nenhum animal cadastrado."
    else mapM_ imprimirAnimal lista