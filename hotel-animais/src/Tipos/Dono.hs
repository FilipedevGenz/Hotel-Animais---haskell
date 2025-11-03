-- Representa a entidade dono no sistema.
-- Tem como atributos: nome, telefone, email e CPF.

module Tipos.Dono where

import Tipos.Common
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Dono = Dono
    { nomeDono      :: Nome
    , telefoneDono  :: Telefone
    , emailDono     :: Email
    , cpfDono       :: CPF  -- Chave Primária (ID)
    } deriving (Show, Eq, Generic)

instance ToJSON Dono
instance FromJSON Dono
