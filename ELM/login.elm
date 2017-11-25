module Login exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (..)

httpErrorString : Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "[ERRO HTTP] Bad Url: " ++ text

        Timeout ->
            "[ERRO HTTP] Timeout"

        NetworkEr              Err y -> (model, Cmd.none)
                Ok  y -> (model, Cmd.none)ror ->
            "[ERRO HTTP] Network Error"

        BadStatus response ->
            "[ERRO HTTP] Status: " ++ toString response.status.code

        BadPayload message response ->
            "[ERRO HTTP] Payload incorreto: "++
             toString message++"("++toString response.status.code++")"

type alias Cadastro =
    {
     email        : String
    ,nome         : String
    ,dtNascimento : String
    ,senha        : String
    ,sexo         : String
    }

type alias Model =
    {
     login : String
    ,senha : String
    ,error : String
    ,cad   : Cadastro
    }

type Message =
      Login String
    | Senha String
    | Submit
    | Response (Result Http.Error Cadastro)

type alias Retorno =
    {
     mensagem : Maybe Cadastro
    ,codigo   : Int
    }


init : Model
init =
    let
        cadIni = Cadastro "" "" "" "" ""
    in
        (Model "" "" "" cadIni)


decodeCad : Decoder Cadastro
decodeCad =  map5 Cadastro (at ["email"] string)
                           (at ["nome"] string)
                           (at ["dtNascimento"] string)
                           (at ["senha"] string)
                           (at ["sexo"] string)

--nao ta em uso ainda..
decodeRetorno : Decoder Retorno
decodeRetorno = map2 Retorno (maybe (at ["mensagem"] decodeCad) )
                             (at ["codigo"] int)


getLogin : String -> String -> Cmd Message
getLogin login senha =
    let
        url = ("https://haskelleta-romefeller.c9users.io/cadastro/busca/"++login++"/"++senha++"/login")
    in
        send Response <| Http.get url decodeCad



viewCad : Cadastro -> Html Message
viewCad c =
    div []
    [
     label [] [text <| toString c.nome]
    ,label [] [text <| toString c.email]
    ,label [] [text <| toString c.senha]
    ,label [] [text <| toString c.dtNascimento]
    ,label [] [text <| toString c.sexo]
    ]

styleLogin : Html.Attribute Message
styleLogin =  style
                [ ("background", "#f4f4f4"),
                  ("padding", "1em"),
                  ("margin-bottom", "10px")
                ]

styleBotaoGO : Html.Attribute Message
styleBotaoGO =  style
                [ ("background", "#009E73"),
                  ("padding", "01em"),
                  ("margin-bottom", "1px"),
                  ("letter-spacing", "5px")
                ]

view : Model -> Html Message
view model =
    div [class "divGeral"]
    [
         input [type_ "text", placeholder "login", required True, onInput Login] []
        ,input [type_ "text", placeholder "senha", required True, onInput Senha] []
        ,button [styleBotaoGO, id "btnEnviar", onClick Submit] [text "GO"]
        ,div [] [text <| toString model.error]
        ,div [] [(viewCad model.cad)]
    ]


main =
  program
    {
     init = (init, Cmd.none)
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
    }
