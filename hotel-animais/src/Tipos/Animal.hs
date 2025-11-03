-- Representa a entidade animal no sistema. 
-- Tem como atributos: ID, nome, idade, espécie, raça, peso e CPF do dono.

module Tipos.Animal where

import Tipos.Common
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Animal = Animal
    { animalID      :: AnimalID -- Chave Primária (ID)
    , nomeAnimal    :: Nome
    , idadeAnimal   :: Idade
    , especieAnimal :: Especie
    , racaAnimal    :: Raca
    , pesoAnimal    :: Float
    , donoCpfAnimal :: CPF      -- Chave Estrangeira para Dono
    } deriving (Show, Eq, Generic)

instance ToJSON Animal
instance FromJSON Animal
