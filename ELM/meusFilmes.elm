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
      -- resp        : List(MeusFilmes)   --meusfilmes
      respMeusFilmes : Resp
     ,favoritos   : List(MeusFilmes)
     ,assistidos  : List(MeusFilmes)
     ,idCadLogado : Int
     ,error       : String
    }

init : Model
init = Model (Resp [] []) [] [] 0 ""

type Message =  SubmitListarMeusFilmes
              | ResponseListarMeusFilmes (Result Http.Error Resp)
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
    Http.send ResponseListarMeusFilmes <| Http.get url (at ["resp"] decodeResp) 


type alias Resp =
  {
    pks : List(Int),
    filmes : List(MeusFilmes)
  }

decodeResp : Decoder Resp
decodeResp = map2 Resp  (at ["pks"] (Json.Decode.list Json.Decode.int))
                        (at ["filmes"] (Json.Decode.list decodeListarMeusFilmes))
                       

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
                Ok y -> ({model | respMeusFilmes = y}, Cmd.none)

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


mostraSingleResp : Int -> MeusFilmes -> Html Message
mostraSingleResp i f = --i é a PK do filmescad, da pra dar update com isso...
    li []
    [
      div [class "poster-filme", class "center-align"]
      [
         img [src (urlFoto++f.poster_path)] []
         ,div [class "lista"] 
         [
          button [class "btn btn-filme"]
          [
           Html.i [class "material-icons small"] [text "star_border"]
          ]
          
          ,button [class "btn btn-filme"]
          [
           Html.i [class "material-icons small"] [text "check"]
          ]
        ]
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
          ,div []
          [
            -- div [class "lista"] (List.map montaItemFilme model.resp)
            ul [class "lista"] (List.map2 mostraSingleResp model.respMeusFilmes.pks model.respMeusFilmes.filmes)
          ]

          ,h1 [onClick SubmitListarFavoritos] [text <|"Favoritos"++model.error]
          ,div []
          [
            ul [class "lista"] (List.map montaItemFilme model.favoritos)
          ]

          ,h1 [onClick SubmitListarAssistidos] [text <|"Assistidos"++model.error]
          ,div []
          [
            ul [class "lista"] (List.map montaItemFilme model.assistidos)
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
