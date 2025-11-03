-- Define as regras de negócio para a entidade Quarto
module Service.QuartoService
    ( adicionarQuarto
    , buscarQuarto
    , atualizarQuarto
    , removerQuarto
    ) where

import Tipos.Common
import Tipos.Quarto
import Tipos.Hotel
import Database.Finders (findQuarto) -- Função de busca reutilizada

-- 'buscarQuarto' é um apelido para a função 'findQuarto' (Maybe Quarto).
buscarQuarto :: QuartoID -> Hotel -> Maybe Quarto
buscarQuarto = findQuarto

-- tenta adicionar um novo quarto ao estado do hotel.
adicionarQuarto :: Quarto -> Hotel -> (Hotel, Either String Quarto)
adicionarQuarto quarto hotel =
    -- REGRA: Verifica se o número do quarto (chave primária) já existe.
    case findQuarto (numeroQuarto quarto) hotel of
        Just _ -> (hotel, Left "Erro: Número do quarto já existe.")
        Nothing ->
            -- add o novo quarto à lista e retorna o estado atualizado.
            let novosQuartos = quarto : quartos hotel
                hotelAtualizado = hotel { quartos = novosQuartos }
            in (hotelAtualizado, Right quarto)

-- atualiza um quarto existente usando uma função 'fn'.
atualizarQuarto :: QuartoID -> (Quarto -> Quarto) -> Hotel -> (Hotel, Either String Quarto)
atualizarQuarto quartoId fn hotel =
    -- Tenta encontrar o quarto pelo ID.
    case findQuarto quartoId hotel of
        Nothing -> (hotel, Left "Erro: Quarto não encontrado.")
        Just quartoAntigo ->
            let 
                -- executa a função de atualização (ex: mudar o tipo do quarto).
                quartoAtualizado = fn quartoAntigo
                
                -- REGRA: Garante que o 'numeroQuarto' (PK) e o status 'ocupado'
                -- não sejam alterados por esta função.
                quartoValidado = quartoAtualizado 
                    { numeroQuarto = numeroQuarto quartoAntigo
                    , ocupado = ocupado quartoAntigo 
                    }
                
                -- recria a lista de quartos com o quarto atualizado.
                outrosQuartos = filter (\q -> numeroQuarto q /= quartoId) (quartos hotel)
                novosQuartos = quartoValidado : outrosQuartos
                hotelAtualizado = hotel { quartos = novosQuartos }
            
            in (hotelAtualizado, Right quartoValidado)

-- remove um quarto do estado do hotel.
removerQuarto :: QuartoID -> Hotel -> (Hotel, Either String ())
removerQuarto quartoId hotel =
    -- tenta encontrar o quarto pelo ID.
    case findQuarto quartoId hotel of
        Nothing -> (hotel, Left "Erro: Quarto não encontrado.")
        Just quarto ->
            -- REGRA: Verifica se o quarto está ocupado.
            if ocupado quarto
            then (hotel, Left "Erro: Não é possível remover um quarto ocupado.")
            else
                -- se estiver livre, usa 'filter' para criar uma nova lista sem ele.
                let quartosRestantes = filter (\q -> numeroQuarto q /= quartoId) (quartos hotel)
                in (hotel { quartos = quartosRestantes }, Right ())