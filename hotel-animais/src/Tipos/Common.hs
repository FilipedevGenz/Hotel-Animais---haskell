-- Criamos um módulo comum para centralizar os tipos algébricos
-- básicos repetidos em várias partes do sistema.

module Tipos.Common where

type Nome = String
type Idade = Int
type Especie = String
type Raca = String
type Telefone = String
type Email = String
type CPF = String      -- ID
type Data = String

type AnimalID = Int
type QuartoID = Int
type ReservaID = Int