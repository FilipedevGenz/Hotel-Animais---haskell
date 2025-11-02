module Tipos.Quarto where

import Tipos.Common
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data TipoQuarto = Simples | Luxo | VIP 
    deriving (Show, Eq, Generic)

instance ToJSON TipoQuarto
instance FromJSON TipoQuarto

data Quarto = Quarto
    { numeroQuarto :: QuartoID -- Chave Primária (ID)
    , tipoQuarto   :: TipoQuarto
    , ocupado      :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON Quarto
instance FromJSON Quarto
