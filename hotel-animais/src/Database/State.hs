module Database.State
    ( initialHotel
    ) where

import Tipos.Hotel
import Tipos.Common (AnimalID, ReservaID)

--inicializacao
initialHotel :: Hotel
initialHotel = Hotel
    { nomeHotel     = "Haskell Pet Hotel"
    , quartos       = []
    , donos         = []
    , animais       = []
    , reservas      = []
    , nextAnimalID  = 1
    , nextReservaID = 1
    }