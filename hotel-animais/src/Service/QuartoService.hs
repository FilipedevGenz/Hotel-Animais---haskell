module Service.QuartoService
    ( adicionarQuarto
    , buscarQuarto
    , atualizarQuarto
    , removerQuarto
    ) where

import Tipos.Common
import Tipos.Quarto
import Tipos.Hotel
import Database.Finders (findQuarto)
-- A importação de Data.List (partition) foi removida, pois 'filter' é mais simples
-- e isso corrige o seu aviso de 'import redundant'

-- FIX: Simplificado para usar o finder
buscarQuarto :: QuartoID -> Hotel -> Maybe Quarto
buscarQuarto = findQuarto

adicionarQuarto :: Quarto -> Hotel -> (Hotel, Either String Quarto)
adicionarQuarto quarto hotel =
    case findQuarto (numeroQuarto quarto) hotel of
        Just _ -> (hotel, Left "Erro: Número do quarto já existe.")
        Nothing ->
            let novosQuartos = quarto : quartos hotel
                hotelAtualizado = hotel { quartos = novosQuartos }
            in (hotelAtualizado, Right quarto)

-- FIX: 'id' renomeado para 'quartoId'
atualizarQuarto :: QuartoID -> (Quarto -> Quarto) -> Hotel -> (Hotel, Either String Quarto)
atualizarQuarto quartoId fn hotel =
    case findQuarto quartoId hotel of
        Nothing -> (hotel, Left "Erro: Quarto não encontrado.")
        Just quartoAntigo ->
            let quartoAtualizado = fn quartoAntigo
                quartoValidado = quartoAtualizado 
                    { numeroQuarto = numeroQuarto quartoAntigo
                    , ocupado = ocupado quartoAntigo 
                    }
                outrosQuartos = filter (\q -> numeroQuarto q /= quartoId) (quartos hotel)
                novosQuartos = quartoValidado : outrosQuartos
                hotelAtualizado = hotel { quartos = novosQuartos }
            in (hotelAtualizado, Right quartoValidado)

-- FIX: 'id' renomeado para 'quartoId' e usando 'filter'
removerQuarto :: QuartoID -> Hotel -> (Hotel, Either String ())
removerQuarto quartoId hotel =
    case findQuarto quartoId hotel of
        Nothing -> (hotel, Left "Erro: Quarto não encontrado.")
        Just quarto ->
            if ocupado quarto
            then (hotel, Left "Erro: Não é possível remover um quarto ocupado.")
            else
                let quartosRestantes = filter (\q -> numeroQuarto q /= quartoId) (quartos hotel)
                in (hotel { quartos = quartosRestantes }, Right ())
