module Tipos.Reserva where

import Tipos.Common

data Reserva = Reserva
    { reservaID     :: ReservaID  -- pk
    , animalIDReserva :: AnimalID -- fk animal
    , quartoIDReserva :: QuartoID -- fk querto
    , dataEntrada   :: Data
    , dataSaida     :: Data
    , precoTotal    :: Float
    } deriving (Show, Eq)