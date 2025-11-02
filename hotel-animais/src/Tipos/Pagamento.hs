module Tipos.Pagamento where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Pagamento
    = Dinheiro Float
    | CartaoCredito Float String
    | Pix Float String
    deriving (Show, Eq, Generic)

instance ToJSON Pagamento
instance FromJSON Pagamento