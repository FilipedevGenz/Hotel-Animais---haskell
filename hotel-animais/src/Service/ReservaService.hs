module Service.ReservaService
    ( adicionarReserva
    , buscarReserva
    , atualizarReserva
    , removerReserva
    ) where

import Tipos.Common
import Tipos.Quarto (Quarto(..))
-- import Tipos.Animal (Animal(..)) -- <--- FIX: Importação removida
import Tipos.Reserva
import Tipos.Hotel
import Database.Finders (findAnimal, findQuarto, findReserva)
import Data.List (partition)

buscarReserva :: ReservaID -> Hotel -> Maybe Reserva
buscarReserva = findReserva

-- FIX: Argumentos renomeados para letras minúsculas (animalId, quartoId)
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

-- (Esta função já estava correta)
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

-- (Esta função já estava correta)
removerReserva :: ReservaID -> Hotel -> (Hotel, Either String ())
removerReserva reservaId hotel =
    case findReserva reservaId hotel of
        Nothing -> (hotel, Left "Erro: Reserva não encontrada.")
        Just reserva ->
            let quartoID = quartoIDReserva reserva
                (quartoAntigo, outrosQuartos) = partition (\q -> numeroQuarto q == quartoID) (quartos hotel)
                quartoLiberado = case quartoAntigo of
                                   (q:_) -> [q { ocupado = False }]
                                   []    -> []
                
                novosQuartos = quartoLiberado ++ outrosQuartos
                reservasRestantes = filter (\r -> reservaID r /= reservaId) (reservas hotel)
                
                hotelAtualizado = hotel 
                    { reservas = reservasRestantes
                    , quartos = novosQuartos 
                    }
            in (hotelAtualizado, Right ())
