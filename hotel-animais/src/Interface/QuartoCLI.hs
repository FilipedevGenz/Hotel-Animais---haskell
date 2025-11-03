-- Menu de "Gerenciar Quartos"
-- Exporta o loop principal e a listagem
module Interface.QuartoCLI (gerenciarQuartos, handleListarQuartos) where

import Tipos.Hotel (Hotel(..), quartos)
import Tipos.Quarto
import Service.QuartoService -- puxa a lógica (adicionarQuarto)
import Interface.Utils (prompt)
import Text.Read (readMaybe) -- pra validar número

-- Loop principal da gerência de quartos.
-- Devolve o hotel atualizado (IO Hotel)
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
            -- checa o resultado do service
            case res of
                Left err  -> putStrLn $ "\nERRO: " ++ err
                Right quarto -> putStrLn $ "\nSucesso! Quarto " ++ show (numeroQuarto quarto) ++ " adicionado."
            gerenciarQuartos novoHotel -- recursão com novo estado
            
        "2" -> handleListarQuartos hotel >> gerenciarQuartos hotel
        "0" -> return hotel -- Ponto de saída!
        _   -> putStrLn "Opção inválida." >> gerenciarQuartos hotel

-- "Formulário" para adicionar um quarto.
handleAdicionarQuarto :: Hotel -> IO (Hotel, Either String Quarto)
handleAdicionarQuarto hotel = do
    putStrLn "--- Adicionar Novo Quarto ---"
    numStr <- prompt "Número do Quarto (ex: 101):"
    tipoStr <- prompt "Tipo (1=Simples, 2=Luxo, 3=VIP):"
    
    let tipoQuarto = case tipoStr of
                        "2" -> Luxo
                        "3" -> VIP
                        _   -> Simples -- default

    case readMaybe numStr of
        Nothing -> return (hotel, Left "Número do quarto inválido.")
        Just numQuarto ->
            -- cria o novo quarto (sempre começa desocupado)
            let novoQuarto = Quarto numQuarto tipoQuarto False
            -- e passa pro 'Service' salvar
            in return $ adicionarQuarto novoQuarto hotel

-- como imprimir um quarto na tela.
imprimirQuarto :: Quarto -> IO ()
imprimirQuarto quarto = do
    putStrLn $ "  Número:   " ++ show (numeroQuarto quarto)
    putStrLn $ "  Tipo:     " ++ show (tipoQuarto quarto)
    putStrLn $ "  Ocupado:  " ++ show (ocupado quarto)
    putStrLn "  --------------------------------"

-- lista todos os quartos
handleListarQuartos :: Hotel -> IO ()
handleListarQuartos hotel = do
    putStrLn "\n--- LISTA DE QUARTOS ---"
    let lista = quartos hotel
    if null lista
    then putStrLn "Nenhum quarto cadastrado."
    else mapM_ imprimirQuarto lista