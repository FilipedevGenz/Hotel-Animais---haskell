-- Define as regras de negócio para a entidade Animal
module Service.AnimalService
    ( adicionarAnimal
    , buscarAnimal
    , atualizarAnimal
    , removerAnimal
    ) where

import Tipos.Common
import Tipos.Animal
import Tipos.Reserva (Reserva(..)) -- Usado para checar se o animal tem reservas
import Tipos.Hotel
import Database.Finders (findDono, findAnimal) -- Funções de busca reutilizadas
import Data.List (partition) -- Para dividir a lista ao remover

-- 'buscarAnimal' é um apelido para a função 'findAnimal' (Maybe Animal).
buscarAnimal :: AnimalID -> Hotel -> Maybe Animal
buscarAnimal = findAnimal

-- tenta adicionar um novo animal ao estado do hotel.
adicionarAnimal :: CPF -> Nome -> Idade -> Especie -> Raca -> Float -> Hotel -> (Hotel, Either String Animal)
adicionarAnimal donoCpf nome idade especie raca peso hotel =
    -- REGRA: Verifica pelo CPF se o dono existe antes de adicionar.
    case findDono donoCpf hotel of
        Nothing -> (hotel, Left "Erro: Dono (CPF) não encontrado.")
        Just _ ->
            let 
                -- Pega o próximo ID disponível e cria o animal
                novoID = nextAnimalID hotel
                novoAnimal = Animal
                    { animalID = novoID
                    , nomeAnimal = nome
                    , idadeAnimal = idade
                    , especieAnimal = especie
                    , racaAnimal = raca
                    , pesoAnimal = peso
                    , donoCpfAnimal = donoCpf
                    }
                
                -- adiciona o novo animal à lista e incrementa o contador de ID.
                novosAnimais = novoAnimal : animais hotel
                hotelAtualizado = hotel { animais = novosAnimais, nextAnimalID = novoID + 1 }
            
            -- retorna o novo estado do hotel e o animal criado.
            in (hotelAtualizado, Right novoAnimal)

-- atualiza um animal existente usando uma função 'fn' (de ordem superior).
atualizarAnimal :: AnimalID -> (Animal -> Animal) -> Hotel -> (Hotel, Either String Animal)
atualizarAnimal animalId fn hotel =
    -- tenta encontrar o animal pelo ID.
    case findAnimal animalId hotel of
        Nothing -> (hotel, Left "Erro: Animal não encontrado.")
        Just animalAntigo ->
            let 
                -- aplica a função de atualização recebida.
                animalAtualizado = fn animalAntigo
                
                -- REGRA: garante que o ID e o CPF do dono não sejam alterados pela 'fn'.
                animalValidado = animalAtualizado { animalID = animalId, donoCpfAnimal = donoCpfAnimal animalAntigo }
                
                -- Recria a lista de animais com o animal atualizado.
                outrosAnimais = filter (\a -> animalID a /= animalId) (animais hotel)
                novosAnimais = animalValidado : outrosAnimais
                hotelAtualizado = hotel { animais = novosAnimais }
            
            in (hotelAtualizado, Right animalValidado)

-- remove um animal do estado do hotel.
removerAnimal :: AnimalID -> Hotel -> (Hotel, Either String ())
removerAnimal animalId hotel =
    -- REGRA: Verifica se o animal tem reservas ativas.
    let reservasDoAnimal = filter (\r -> animalIDReserva r == animalId) (reservas hotel)
    in if not (null reservasDoAnimal)
        then (hotel, Left "Erro: Não é possível remover animal com reservas ativas.")
        else
            -- Separa a lista entre o animal a ser removido ('removidos') e os 'restantes'.
            let (removidos, restantes) = partition (\a -> animalID a == animalId) (animais hotel)
            in if null removidos
                -- se 'removidos' está vazio, quer dizer que o animal não foi encontrado.
                then (hotel, Left "Erro: Animal não encontrado.")
                -- se foi encontrado, atualiza o hotel para conter apenas os 'restantes'.
                else (hotel { animais = restantes }, Right ())