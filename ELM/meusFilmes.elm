module MeusFilmes exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import BuscaFilme as BF exposing(..)

type alias MeusFilmes =
    {
     id                   : Int
    ,title               : String
    ,vote_average        : Float
    ,poster_path         : String
    ,overview            : String
    ,release_date        : String
    }


type alias Model =
    {
      resp        : List(MeusFilmes)   --meusfilmes
     ,favoritos   : List(MeusFilmes)
     ,assistidos  : List(MeusFilmes)
     ,idCadLogado : Int
     ,error       : String
    }

init : Model
init = Model [] [] [] 0 ""

type Message =  SubmitListarMeusFilmes
              | ResponseListarMeusFilmes (Result Http.Error (List(MeusFilmes)))
              | SubmitListarFavoritos
              | ResponseListarFavoritos (Result Http.Error (List(MeusFilmes)))
              | SubmitListarAssistidos
              | ResponseListarAssistidos (Result Http.Error (List(MeusFilmes)))

urlFoto : String
urlFoto = "http://image.tmdb.org/t/p/w342/"


decodeListarMeusFilmes : Decoder MeusFilmes
decodeListarMeusFilmes = map6 MeusFilmes (at ["idapi"] int)
                                         (at ["title"] string)
                                         (at ["vote_average"] float)
                                         (at ["poster_path"] string)
                                         (at ["overview"] string)
                                         (at ["release_date"] string)


getListarMeusFilmes : Int -> Cmd Message
getListarMeusFilmes idcad =
  let
    url = ("https://haskelleta-romefeller.c9users.io/filmescad/listarfilmes/"++ toString idcad)
  in
    Http.send ResponseListarMeusFilmes <| Http.get url (at ["resp"] (Json.Decode.list decodeListarMeusFilmes))


getListarFavoritos : Int -> Cmd Message
getListarFavoritos idcad =
  let
    url = ("https://haskelleta-romefeller.c9users.io/filmescad/listarfavoritos/"++ toString idcad)
  in
    Http.send ResponseListarFavoritos <| Http.get url (at ["resp"] (Json.Decode.list decodeListarMeusFilmes))


getListarAssistidos : Int -> Cmd Message
getListarAssistidos idcad =
  let
    url = ("https://haskelleta-romefeller.c9users.io/filmescad/listarassistidos/"++ toString idcad)
  in
    Http.send ResponseListarAssistidos <| Http.get url (at ["resp"] (Json.Decode.list decodeListarMeusFilmes))


update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        SubmitListarMeusFilmes ->
            (model, getListarMeusFilmes model.idCadLogado)

        ResponseListarMeusFilmes x ->
            case x of
                Err y -> ({model | error = toString y} , Cmd.none)
                Ok y -> ({model | resp = y}, Cmd.none)

        SubmitListarFavoritos ->
            (model, getListarFavoritos model.idCadLogado)

        ResponseListarFavoritos x ->
            case x of
                Err y -> ({model | error = toString y} , Cmd.none)
                Ok y -> ({model | favoritos = y}, Cmd.none)

        SubmitListarAssistidos ->
            (model, getListarAssistidos model.idCadLogado)

        ResponseListarAssistidos x ->
            case x of
                Err y -> ({model | error = toString y} , Cmd.none)
                Ok y -> ({model | assistidos = y}, Cmd.none)


montaItemFilme : MeusFilmes -> Html Message
montaItemFilme mf =
            li []  -- CRIAR UM LI NESSE ESTILO PARA CADA FILME
            [
              div [class "poster-filme"]
              [
                img [src (urlFoto++mf.poster_path)] []
              ]
            ]


view : Model -> Html Message
view model =
  div [class "row"]
  [
    div [class "col s12 m8 l9"]
    [
      section []
      [
          h1 [onClick SubmitListarMeusFilmes] [text <|"Todos os filmes"++model.error]
          ,ul []
          [
            div [class "lista"] (List.map montaItemFilme model.resp)
          ]

          ,h1 [onClick SubmitListarFavoritos] [text <|"Favoritos"++model.error]
          ,ul []
          [
            div [class "lista"] (List.map montaItemFilme model.favoritos)
          ]

          ,h1 [onClick SubmitListarAssistidos] [text <|"Assistidos"++model.error]
          ,ul []
          [
            div [class "lista"] (List.map montaItemFilme model.assistidos)
          ]
      ]
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
