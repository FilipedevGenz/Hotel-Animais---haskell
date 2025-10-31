module Tipos.Reserva where

import Tipos.Common

data Reserva = Reserva
    { reservaID     :: ReservaID  -- PK
    , animalIDReserva :: AnimalID -- FK Animal
    , quartoIDReserva :: QuartoID -- FK Quarto
    , dataEntrada   :: Data
    , dataSaida     :: Data
    , precoTotal    :: Float
    } deriving (Show, Eq)