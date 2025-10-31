module Database.Finders
    ( findDono
    , findAnimal
    , findQuarto
    , findReserva
    ) where

import Tipos.Common
import Tipos.Dono
import Tipos.Quarto
import Tipos.Animal
import Tipos.Reserva
import Tipos.Hotel
import Data.List (find)

findDono :: CPF -> Hotel -> Maybe Dono
findDono cpf hotel = find (\d -> cpfDono d == cpf) (donos hotel)

findAnimal :: AnimalID -> Hotel -> Maybe Animal
findAnimal id hotel = find (\a -> animalID a == id) (animais hotel)

findQuarto :: QuartoID -> Hotel -> Maybe Quarto
findQuarto id hotel = find (\q -> numeroQuarto q == id) (quartos hotel)

findReserva :: ReservaID -> Hotel -> Maybe Reserva
findReserva id hotel = find (\r -> reservaID r == id) (reservas hotel)