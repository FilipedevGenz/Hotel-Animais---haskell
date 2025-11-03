module Service.DonoService
    ( adicionarDono
    , buscarDono
    , atualizarDono
    , removerDono
    ) where

import Tipos.Common
import Tipos.Dono
import Tipos.Animal (Animal(..))
import Tipos.Hotel
import Database.Finders (findDono)
import Data.List (partition)

buscarDono :: CPF -> Hotel -> Maybe Dono
buscarDono = findDono

adicionarDono :: Dono -> Hotel -> (Hotel, Either String Dono)
adicionarDono dono hotel =
    case findDono (cpfDono dono) hotel of
        Just _ -> (hotel, Left "Erro: CPF já cadastrado.")
        Nothing -> 
            let novosDonos = dono : donos hotel
                hotelAtualizado = hotel { donos = novosDonos }
            in (hotelAtualizado, Right dono)

atualizarDono :: CPF -> (Dono -> Dono) -> Hotel -> (Hotel, Either String Dono)
atualizarDono cpf fn hotel =
    case findDono cpf hotel of
        Nothing -> (hotel, Left "Erro: Dono não encontrado.")
        Just donoAntigo ->
            let donoAtualizado = fn donoAntigo
                donoValidado = donoAtualizado { cpfDono = cpfDono donoAntigo }
                
                -- MUDANÇA AQUI: 'filter' foi substituído pela compreensão de lista
                outrosDonos = [d | d <- donos hotel, cpfDono d /= cpf]
                
                novosDonos = donoValidado : outrosDonos
                hotelAtualizado = hotel { donos = novosDonos }
            in (hotelAtualizado, Right donoValidado)

removerDono :: CPF -> Hotel -> (Hotel, Either String ())
removerDono cpf hotel =
    let animaisDoDono = filter (\a -> donoCpfAnimal a == cpf) (animais hotel)
    in if not (null animaisDoDono)
        then (hotel, Left "Erro: Não é possível remover dono com animais cadastrados.")
        else 
            let (removidos, restantes) = partition (\d -> cpfDono d == cpf) (donos hotel)
            in if null removidos
                then (hotel, Left "Erro: Dono não encontrado.")
                else (hotel { donos = restantes }, Right ())