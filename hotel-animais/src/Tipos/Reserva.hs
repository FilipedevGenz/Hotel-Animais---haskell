module Tipos.Reserva where

import Tipos.Common
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Reserva = Reserva
    { reservaID     :: ReservaID  -- Chave Primária (ID)
    , animalIDReserva :: AnimalID -- Chave Estrangeira para Animal
    , quartoIDReserva :: QuartoID -- Chave Estrangeira para Quarto
    , dataEntrada   :: Data
    , dataSaida     :: Data
    , precoTotal    :: Float
    } deriving (Show, Eq, Generic)

instance ToJSON Reserva
instance FromJSON Reserva
