-- O menu que cuida só dos animais
-- Exporta o loop principal 'gerenciarAnimais' e a função de listagem

module Interface.AnimalCLI (gerenciarAnimais, handleListarAnimais) where

import Tipos.Hotel (Hotel(..), animais) -- Pega o estado do hotel e o "getter" de animais
import Tipos.Animal
import Tipos.Common
import Service.AnimalService -- Puxa as funções com a lógica (adicionar, etc)
import Interface.Utils (prompt) 
import Text.Read (readMaybe) -- Para converter String para Int/Float com segurança

-- loop principal do menu de animais.
-- ele recebe o hotel, faz as operações, e devolve o hotel atualizado (IO Hotel)
gerenciarAnimais :: Hotel -> IO Hotel
gerenciarAnimais hotel = do
    putStrLn "\n=== Gerenciar Animais ==="
    putStrLn "1. Adicionar Animal"
    putStrLn "2. Listar Animais"
    putStrLn "0. Voltar"
    putStrLn "========================="

    opcao <- prompt "Escolha:"
    
    -- swich case pra tratar a escolha do usuário
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarAnimal hotel
            -- checa se o 'service' retornou um erro (Left) ou sucesso (Right)
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right animal -> putStrLn $ "\nSucesso! Animal '" ++ nomeAnimal animal ++ "' adicionado."
            gerenciarAnimais novoHotel

        "2" -> handleListarAnimais hotel >> gerenciarAnimais hotel -- só lista e chama o menu de novo
        "0" -> return hotel -- Ponto de saída. Devolve o 'hotel' pro 'appLoop' principal
        _   -> putStrLn "Opção inválida." >> gerenciarAnimais hotel -- se digitar qualquer outra coisa

-- Pede todos os dados do animal pro usuário
handleAdicionarAnimal :: Hotel -> IO (Hotel, Either String Animal)
handleAdicionarAnimal hotel = do
    putStrLn "--- Adicionar Novo Animal ---"
    cpfDono <- prompt "CPF do Dono:"
    nome <- prompt "Nome do Animal:"
    idadeStr <- prompt "Idade:"
    especie <- prompt "Espécie (ex: Cachorro):"
    raca <- prompt "Raça (ex: Poodle):"
    pesoStr <- prompt "Peso (ex: 8.5):"
   
    -- usa 'readMaybe' pra não quebrar o programa se digitarem texto em vez de número.
    case (readMaybe idadeStr, readMaybe pesoStr) of
        (Nothing, _) -> return (hotel, Left "Idade inválida.")
        (_, Nothing) -> return (hotel, Left "Peso inválido.")
        (Just idade, Just peso) ->
            -- Se os números são válidos, repassa tudo pro 'Service'
            -- O Service que vai checar se o dono existe e salvar.
            return $ adicionarAnimal cpfDono nome idade especie raca peso hotel

-- função de impressão de um animal na tela
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

-- Pega a lista de animais e imprime um por um.
handleListarAnimais :: Hotel -> IO ()
handleListarAnimais hotel = do
    putStrLn "\n--- LISTA DE ANIMAIS ---"
    let lista = animais hotel
    
    -- checa se a lista tá vazia antes de tentar imprimir.
    if null lista
    then putStrLn "Nenhum animal cadastrado."
    -- 'mapM_' aplica o 'imprimirAnimal' em cada item da lista.
    else mapM_ imprimirAnimal lista