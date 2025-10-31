# Hotel-Animais---haskell
Projeto da disciplina de paradigmas da programacao com objetivo de estudo da linguagem haskell e paradigma de programacao fucional.
# Projeto: Hotel de Animais (CRUD em Haskell)

Sistema de *Gerenciamento de um hotel de animais de estimação, escrito inteiramente em Haskell.

O sistema é capaz de gerenciar:
* **Donos** (Clientes)
* **Animais**
* **Quartos** do hotel
* **Reservas** (ligando animais a quartos)

Toda a lógica é implementada de forma funcional, e o estado do hotel é persistido em um arquivo `hotel.json` local.

## Funcionalidades

* **CRUD de Donos:** Adicionar, buscar, atualizar e remover donos (validado por CPF).
* **CRUD de Animais:** Adicionar, buscar, atualizar e remover animais (ligados a um dono).
* **CRUD de Quartos:** Adicionar, buscar, atualizar e remover quartos (validado por número).
* **CRUD de Reservas:** Adicionar (fazer check-in) e remover (fazer check-out) reservas.
* **Validação de Regras de Negócio:**
    * Não permite cadastrar CPFs ou números de quarto duplicados.
    * Não permite reservar um quarto que já está ocupado.
    * Não permite remover um dono que possua animais cadastrados.
    * Não permite remover um animal que possua uma reserva ativa.
* **Persistência de Dados:** O estado completo do hotel (listas de donos, animais, quartos e reservas) é automaticamente salvo em um arquivo `hotel.json` formatado sempre que o programa é encerrado, e recarregado quando ele inicia.

## Stack Utilizada

* **Linguagem:** [Haskell](https://www.haskell.org/) (GHC)
* **Gerenciador de Pacotes:** [Cabal](https://www.haskell.org/cabal/)
* **Persistência:** Arquivo JSON local
* **Bibliotecas Principais:**
    * `base`: Biblioteca padrão do Haskell.
    * `aeson`: Para codificação (serialização) e decodificação (parsing) de dados para o formato JSON.
    * `bytestring`: Para leitura e escrita eficiente de arquivos.

## OBS:

Após a primeira execução, um arquivo chamado `hotel.json` será criado na raiz do projeto. Este arquivo *é* o seu banco de dados. Você pode abri-lo para inspecionar o estado do hotel.

O código-fonte está localizado inteiramente na pasta `src/` e é organizado da seguinte forma:
