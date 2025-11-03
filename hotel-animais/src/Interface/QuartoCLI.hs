module Interface.QuartoCLI (gerenciarQuartos, handleListarQuartos) where

-- Imports de Tipos
import Tipos.Hotel (Hotel(..), quartos)
import Tipos.Quarto

-- Imports de Serviços
import Service.QuartoService

-- Imports de Utilitários
import Interface.Utils (prompt)
import Text.Read (readMaybe)

-- Loop principal da gerência de quartos
-- Retorna o estado atualizado do hotel
gerenciarQuartos :: Hotel -> IO Hotel
gerenciarQuartos hotel = do
    putStrLn "\n=== Gerenciar Quartos ==="
    putStrLn "1. Adicionar Quarto"
    putStrLn "2. Listar Quartos"
    putStrLn "0. Voltar"
    putStrLn "========================="

    opcao <- prompt "Escolha:"
    
    case opcao of
        "1" -> do
            (novoHotel, res) <- handleAdicionarQuarto hotel
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right quarto -> putStrLn $ "\nSucesso! Quarto " ++ show (numeroQuarto quarto) ++ " adicionado."
            gerenciarQuartos novoHotel -- Chama a si mesmo recursivamente
            
        "2" -> handleListarQuartos hotel >> gerenciarQuartos hotel
        "0" -> return hotel -- Retorna o estado atualizado para o loop principal
        _   -> putStrLn "Opção inválida." >> gerenciarQuartos hotel

-- Handlers
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

-- Funções de Listagem
imprimirQuarto :: Quarto -> IO ()
imprimirQuarto quarto = do
    putStrLn $ "  Número:   " ++ show (numeroQuarto quarto)
    putStrLn $ "  Tipo:     " ++ show (tipoQuarto quarto)
    putStrLn $ "  Ocupado:  " ++ show (ocupado quarto)
    putStrLn "  --------------------------------" -- Separador

handleListarQuartos :: Hotel -> IO ()
handleListarQuartos hotel = do
    putStrLn "\n--- LISTA DE QUARTOS ---"
    let lista = quartos hotel
    if null lista
    then putStrLn "Nenhum quarto cadastrado."
    else mapM_ imprimirQuarto lista