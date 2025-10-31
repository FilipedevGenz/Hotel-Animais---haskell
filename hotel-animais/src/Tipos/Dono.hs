module Tipos.Dono where

import Tipos.Common

data Dono = Dono
    { nomeDono      :: Nome
    , telefoneDono  :: Telefone
    , emailDono     :: Email
    , cpfDono       :: CPF  -- ID
    } deriving (Show, Eq)