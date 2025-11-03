module Service.ReservaService
    ( adicionarReserva
    , buscarReserva
    , atualizarReserva
    , removerReserva
    ) where

import Tipos.Common
import Tipos.Quarto (Quarto(..))
import Tipos.Reserva
import Tipos.Hotel
import Database.Finders (findAnimal, findQuarto, findReserva)
-- MUDANÇA AQUI: 'partition' não é mais necessário, mas 'map' (do Prelude) será usado.
import Data.List (partition, filter) 

buscarReserva :: ReservaID -> Hotel -> Maybe Reserva
buscarReserva = findReserva

adicionarReserva :: AnimalID -> QuartoID -> Data -> Data -> Float -> Hotel -> (Hotel, Either String Reserva)
adicionarReserva animalId quartoId dtEntrada dtSaida preco hotel =
    case (findAnimal animalId hotel, findQuarto quartoId hotel) of
        (Nothing, _) -> (hotel, Left "Erro: Animal não encontrado.")
        (_, Nothing) -> (hotel, Left "Erro: Quarto não encontrado.")
        (Just _, Just quarto) ->
            if ocupado quarto
            then (hotel, Left "Erro: Quarto já está ocupado.")
            else
                let novoID = nextReservaID hotel
                    novaReserva = Reserva
                        { reservaID = novoID
                        , animalIDReserva = animalId
                        , quartoIDReserva = quartoId
                        , dataEntrada = dtEntrada
                        , dataSaida = dtSaida
                        , precoTotal = preco
                        }
                    
                    quartoOcupado = quarto { ocupado = True }
                    outrosQuartos = filter (\q -> numeroQuarto q /= quartoId) (quartos hotel)
                    novosQuartos = quartoOcupado : outrosQuartos
                    
                    novasReservas = novaReserva : reservas hotel
                    
                    hotelAtualizado = hotel 
                        { reservas = novasReservas
                        , quartos = novosQuartos
                        , nextReservaID = novoID + 1 
                        }
                in (hotelAtualizado, Right novaReserva)

atualizarReserva :: ReservaID -> (Reserva -> Reserva) -> Hotel -> (Hotel, Either String Reserva)
atualizarReserva reservaId fn hotel =
    case findReserva reservaId hotel of
        Nothing -> (hotel, Left "Erro: Reserva não encontrada.")
        Just reservaAntiga ->
            let reservaAtualizada = fn reservaAntiga
                reservaValidada = reservaAtualizada
                    { reservaID = reservaID reservaAntiga
                    , animalIDReserva = animalIDReserva reservaAntiga
                    , quartoIDReserva = quartoIDReserva reservaAntiga
                    }
                outrasReservas = filter (\r -> reservaID r /= reservaId) (reservas hotel)
                novasReservas = reservaValidada : outrasReservas
                hotelAtualizado = hotel { reservas = novasReservas }
            in (hotelAtualizado, Right reservaValidada)

removerReserva :: ReservaID -> Hotel -> (Hotel, Either String ())
removerReserva reservaId hotel =
    case findReserva reservaId hotel of
        Nothing -> (hotel, Left "Erro: Reserva não encontrada.")
        Just reserva ->
            let 
                quartoID = quartoIDReserva reserva
                
                -- MUDANÇA AQUI (Início)
                -- 1. Criamos uma função que sabe como liberar UM quarto
                liberarQuarto q = if numeroQuarto q == quartoID
                                  then q { ocupado = False } -- Libera o quarto
                                  else q                   -- Mantém o quarto como está

                -- 2. Usamos 'map' para aplicar essa função a TODOS os quartos
                novosQuartos = map liberarQuarto (quartos hotel)
                -- MUDANÇA AQUI (Fim)

                reservasRestantes = filter (\r -> reservaID r /= reservaId) (reservas hotel)
                
                hotelAtualizado = hotel 
                    { reservas = reservasRestantes
                    , quartos = novosQuartos 
                    }
            in (hotelAtualizado, Right ())