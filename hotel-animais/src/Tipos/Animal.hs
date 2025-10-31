module Tipos.Animal where

import Tipos.Common

data Animal = Animal
    { animalID      :: AnimalID -- PK
    , nomeAnimal    :: Nome
    , idadeAnimal   :: Idade
    , especieAnimal :: Especie
    , racaAnimal    :: Raca
    , pesoAnimal    :: Float
    , donoCpfAnimal :: CPF      -- FK
    } deriving (Show, Eq)