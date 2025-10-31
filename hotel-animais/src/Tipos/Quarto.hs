module Tipos.Quarto where

import Tipos.Common

data TipoQuarto = Simples | Luxo | VIP deriving (Show, Eq)

data Quarto = Quarto
    { numeroQuarto :: QuartoID
    , tipoQuarto   :: TipoQuarto
    , ocupado      :: Bool
    } deriving (Show, Eq)