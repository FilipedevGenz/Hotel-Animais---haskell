-- Define as regras de negócio para a entidade Dono
module Service.DonoService
    ( adicionarDono
    , buscarDono
    , atualizarDono
    , removerDono
    ) where

import Tipos.Common
import Tipos.Dono
import Tipos.Animal (Animal(..)) -- Usado para checar se o dono possui animais
import Tipos.Hotel
import Database.Finders (findDono) -- Função de busca reutilizada
import Data.List (partition) -- Para dividir a lista ao remover

-- 'buscarDono' é um apelido para a função 'findDono' (Maybe Dono).
buscarDono :: CPF -> Hotel -> Maybe Dono
buscarDono = findDono

-- Tenta adicionar um novo dono ao estado do hotel.
adicionarDono :: Dono -> Hotel -> (Hotel, Either String Dono)
adicionarDono dono hotel =
    -- REGRA: Verifica se o CPF do dono já existe.
    case findDono (cpfDono dono) hotel of
        Just _ -> (hotel, Left "Erro: CPF já cadastrado.")
        Nothing -> 
            -- add o novo dono à lista e retorna o estado atualizado.
            let novosDonos = dono : donos hotel
                hotelAtualizado = hotel { donos = novosDonos }
            in (hotelAtualizado, Right dono)

-- atualiza um dono existente usando uma função 'fn'.
atualizarDono :: CPF -> (Dono -> Dono) -> Hotel -> (Hotel, Either String Dono)
atualizarDono cpf fn hotel =
    -- Tenta encontrar o dono pelo CPF.
    case findDono cpf hotel of
        Nothing -> (hotel, Left "Erro: Dono não encontrado.")
        Just donoAntigo ->
            let 
                -- aplica a função de atualização recebida.
                donoAtualizado = fn donoAntigo
                
                -- REGRA: Garante que o CPF (chave primária) não seja alterado pela 'fn'.
                donoValidado = donoAtualizado { cpfDono = cpfDono donoAntigo }
                
                -- recria a lista de donos usando compreensão de lista.
                -- seleciona todos os donos 'd' ONDE o CPF for diferente (!=) do CPF atual.
                outrosDonos = [d | d <- donos hotel, cpfDono d /= cpf]
                
                -- adiciona o dono validado de volta à lista.
                novosDonos = donoValidado : outrosDonos
                hotelAtualizado = hotel { donos = novosDonos }
            in (hotelAtualizado, Right donoValidado)

-- remove um dono do estado do hotel.
removerDono :: CPF -> Hotel -> (Hotel, Either String ())
removerDono cpf hotel =
    -- REGRA: Verifica se o dono possui algum animal cadastrado.
    let animaisDoDono = filter (\a -> donoCpfAnimal a == cpf) (animais hotel)
    in if not (null animaisDoDono)
        then (hotel, Left "Erro: Não é possível remover dono com animais cadastrados.")
        else 
            -- Separa a lista entre o dono a ser removido ('removidos') e os 'restantes'.
            let (removidos, restantes) = partition (\d -> cpfDono d == cpf) (donos hotel)
            in if null removidos
                -- se 'removidos' está vazio, o dono não foi encontrado.
                then (hotel, Left "Erro: Dono não encontrado.")
                -- se foi encontrado, atualiza o hotel para conter apenas os 'restantes'.
                else (hotel { donos = restantes }, Right ())