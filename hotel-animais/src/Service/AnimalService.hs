module Service.AnimalService
    ( adicionarAnimal
    , buscarAnimal
    , atualizarAnimal
    , removerAnimal
    ) where

import Tipos.Common
-- import Tipos.Dono (Dono(..)) -- <--- FIX: Importação removida
import Tipos.Animal
import Tipos.Reserva (Reserva(..))
import Tipos.Hotel
import Database.Finders (findDono, findAnimal)
import Data.List (partition)

buscarAnimal :: AnimalID -> Hotel -> Maybe Animal
buscarAnimal = findAnimal

adicionarAnimal :: CPF -> Nome -> Idade -> Especie -> Raca -> Float -> Hotel -> (Hotel, Either String Animal)
adicionarAnimal donoCpf nome idade especie raca peso hotel =
    case findDono donoCpf hotel of
        Nothing -> (hotel, Left "Erro: Dono (CPF) não encontrado.")
        Just _ ->
            let novoID = nextAnimalID hotel
                novoAnimal = Animal
                    { animalID = novoID
                    , nomeAnimal = nome
                    , idadeAnimal = idade
                    , especieAnimal = especie
                    , racaAnimal = raca
                    , pesoAnimal = peso
                    , donoCpfAnimal = donoCpf
                    }
                novosAnimais = novoAnimal : animais hotel
                hotelAtualizado = hotel { animais = novosAnimais, nextAnimalID = novoID + 1 }
            in (hotelAtualizado, Right novoAnimal)

atualizarAnimal :: AnimalID -> (Animal -> Animal) -> Hotel -> (Hotel, Either String Animal)
atualizarAnimal animalId fn hotel =
    case findAnimal animalId hotel of
        Nothing -> (hotel, Left "Erro: Animal não encontrado.")
        Just animalAntigo ->
            let animalAtualizado = fn animalAntigo
                animalValidado = animalAtualizado { animalID = animalId, donoCpfAnimal = donoCpfAnimal animalAntigo }
                outrosAnimais = filter (\a -> animalID a /= animalId) (animais hotel)
                novosAnimais = animalValidado : outrosAnimais
                hotelAtualizado = hotel { animais = novosAnimais }
            in (hotelAtualizado, Right animalValidado)

removerAnimal :: AnimalID -> Hotel -> (Hotel, Either String ())
removerAnimal animalId hotel =
    let reservasDoAnimal = filter (\r -> animalIDReserva r == animalId) (reservas hotel)
    in if not (null reservasDoAnimal)
        then (hotel, Left "Erro: Não é possível remover animal com reservas ativas.")
        else
            let (removidos, restantes) = partition (\a -> animalID a == animalId) (animais hotel)
            in if null removidos
                then (hotel, Left "Erro: Animal não encontrado.")
                else (hotel { animais = restantes }, Right ())
