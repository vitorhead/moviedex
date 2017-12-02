module MeusFilmes exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import BuscaFilme as BF exposing(..)

type alias Model =
    {
     meusFilmes : MeusFilmes
    }

type alias MeusFilmes =
    {
        resp : List (BF.FilmeResult)
    }

init : Model
init = Model <| []


decodeMeusFilmes : Decoder MeusFilmes
decodeMeusFilmes = Json.Decode.map MeusFilmes (at ["resp"] (Json.Decode.list BF.decodeFilmeResult))

getMeusFilmes : String -> Cmd Message
getMeusFilmes idcad =
    let
        url = ("https://haskelleta-romefeller.c9users.io/filmescad/listarfilmes/"++idcad)
    in
        send Response <| Http.get url decodeMeusFilmes

view : Model -> Html Message
view model =
  div [class "row"]
  [
    div [class "sidebar col s12 m4 l3"]
    [
      div [class "lateral-principal"]
      [
        ul []
        [
          li [] [text "NOME CHAMPS"]
          ,li []
          [
            a [class "btn green"] [text "Buscar Filmes"]
          ]
          ,li []
          [
            a [class "btn red"] [text "Deslogar"]
          ]
        ]
      ]
    ]
    ,div [class "col s12 m8 l9"]
    [
      section []
      [
        h1 [] [text "NOME LISTA"]
        ,ul [class "lista"]
        [
          li []  -- CRIAR UM LI NESSE ESTILO PARA CADA FILME
          [
            div [class "poster-filme"]
            [
              img [onClick ] [] --Mudar Pg-DETALHES
            ]
          ]
          ,li []  -- CRIAR UM LI NESSE ESTILO PARA CADA FILME
          [
            div [class "poster-filme"]
            [
              img [onClick ] [] --Mudar Pg-DETALHES
            ]
          ]
        ] -- fim ul
      ] -- fim section
    ]
  ]


main =
  program
    {
     init = (init, Cmd.none)
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
    }
