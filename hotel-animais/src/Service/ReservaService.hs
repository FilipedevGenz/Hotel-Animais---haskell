-- Define as regras de negócio para a entidade Reserva (check-in e check-out)
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
import Database.Finders (findAnimal, findQuarto, findReserva) -- Funções de busca
import Data.List (partition, filter, map) -- 'map' é usado para atualizar o quarto

-- 'buscarReserva' é um apelido para a função 'findReserva' (Maybe Reserva).
buscarReserva :: ReservaID -> Hotel -> Maybe Reserva
buscarReserva = findReserva

-- tenta adicionar uma nova reserva (fazer check-in).
adicionarReserva :: AnimalID -> QuartoID -> Data -> Data -> Float -> Hotel -> (Hotel, Either String Reserva)
adicionarReserva animalId quartoId dtEntrada dtSaida preco hotel =
    -- REGRA: Checa se o Animal (FK) e o Quarto (FK) existem.
    case (findAnimal animalId hotel, findQuarto quartoId hotel) of
        (Nothing, _) -> (hotel, Left "Erro: Animal não encontrado.")
        (_, Nothing) -> (hotel, Left "Erro: Quarto não encontrado.")
        (Just _, Just quarto) ->
            -- REGRA: Checa se o quarto já está ocupado.
            if ocupado quarto
            then (hotel, Left "Erro: Quarto já está ocupado.")
            else
                -- se tudo for válido, cria a nova reserva:
                let novoID = nextReservaID hotel
                    novaReserva = Reserva
                        { reservaID = novoID
                        , animalIDReserva = animalId
                        , quartoIDReserva = quartoId
                        , dataEntrada = dtEntrada
                        , dataSaida = dtSaida
                        , precoTotal = preco
                        }
                    
                    -- Marca o quarto como ocupado.
                    quartoOcupado = quarto { ocupado = True }
                    -- Recria a lista de quartos com o quarto atualizado.
                    outrosQuartos = filter (\q -> numeroQuarto q /= quartoId) (quartos hotel)
                    novosQuartos = quartoOcupado : outrosQuartos
                    
                    -- Adiciona a nova reserva à lista.
                    novasReservas = novaReserva : reservas hotel
                    
                    -- Atualiza o estado do hotel com as duas listas novas e o próximo ID.
                    hotelAtualizado = hotel 
                        { reservas = novasReservas
                        , quartos = novosQuartos
                        , nextReservaID = novoID + 1 
                        }
                in (hotelAtualizado, Right novaReserva)

-- atualiza dados de uma reserva existente usando uma função 'fn'.
atualizarReserva :: ReservaID -> (Reserva -> Reserva) -> Hotel -> (Hotel, Either String Reserva)
atualizarReserva reservaId fn hotel =
    -- verifica se a reserva existe.
    case findReserva reservaId hotel of
        Nothing -> (hotel, Left "Erro: Reserva não encontrada.")
        Just reservaAntiga ->
            let 
                -- faz a aplicação da função de atualização.
                reservaAtualizada = fn reservaAntiga
                
                -- REGRA: Garante que IDs e chaves estrangeiras (animal/quarto)
                -- não sejam alterados pela função 'fn'.
                reservaValidada = reservaAtualizada
                    { reservaID = reservaID reservaAntiga
                    , animalIDReserva = animalIDReserva reservaAntiga
                    , quartoIDReserva = quartoIDReserva reservaAntiga
                    }
                
                -- faz a recriação da lista de reservas com a versão atualizada.
                outrasReservas = filter (\r -> reservaID r /= reservaId) (reservas hotel)
                novasReservas = reservaValidada : outrasReservas
                hotelAtualizado = hotel { reservas = novasReservas }
            
            in (hotelAtualizado, Right reservaValidada)

-- Remove uma reserva (faz check-out) e libera o quarto.
removerReserva :: ReservaID -> Hotel -> (Hotel, Either String ())
removerReserva reservaId hotel =
    -- Verifica se a reserva existe.
    case findReserva reservaId hotel of
        Nothing -> (hotel, Left "Erro: Reserva não encontrada.")
        Just reserva ->
            let 
                -- Encontra o quarto que estava associado a esta reserva.
                quartoID = quartoIDReserva reserva
                
                -- Libera o quarto.
                -- Função auxiliar que libera o quarto se o ID bater.
                liberarQuarto q = if numeroQuarto q == quartoID
                                  then q { ocupado = False } -- Libera o quarto
                                  else q                   -- Mantém o quarto como está

                -- Usa map para aplicar a função 'liberarQuarto'
                -- a toda a lista de quartos, criando a nova lista.
                novosQuartos = map liberarQuarto (quartos hotel)

                -- Usa filter para criar a nova lista de reservas (sem a removida).
                reservasRestantes = filter (\r -> reservaID r /= reservaId) (reservas hotel)
                
                -- Atualiza o estado do hotel com as duas novas listas.
                hotelAtualizado = hotel 
                    { reservas = reservasRestantes
                    , quartos = novosQuartos 
                    }
            in (hotelAtualizado, Right ())