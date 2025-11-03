-- Representa a entidade hotel no sistema.
-- Tem como atributos: nome, lista de quartos, donos, animais, reservas e 
-- IDs para novos animais e reservas.

module Tipos.Hotel where

import Tipos.Quarto
import Tipos.Reserva
import Tipos.Animal
import Tipos.Dono
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Hotel = Hotel
    { nomeHotel     :: String
    , quartos       :: [Quarto]
    , donos         :: [Dono]
    , animais       :: [Animal]
    , reservas      :: [Reserva]
    , nextAnimalID  :: Int
    , nextReservaID :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON Hotel
instance FromJSON Hotel
