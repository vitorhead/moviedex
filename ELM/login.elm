module Login exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (..)
import Sha256 as SHA exposing (sha256)

httpErrorString : Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "[ERRO HTTP] Bad Url: " ++ text

        Timeout ->
            "[ERRO HTTP] Timeout"

        NetworkError -> 
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
    ,ret   : Retorno
    }

type Message =
      Login String
    | Senha String
    | Submit
    | Response (Result Http.Error Retorno)

type alias Retorno =
    {
      codigo : Int
    , mensagem : Mensagem
    }

type alias Mensagem =
    {
        autenticacao : String
    ,   idcadastro   : Int
    ,   resp         : String
    }

init : Model
init =
    let
        retIni = Retorno 0 (Mensagem "" 0 "")
    in
        (Model "" "" "" retIni)


decodeCad : Decoder Cadastro
decodeCad =  map5 Cadastro (at ["email"] string)
                           (at ["nome"] string)
                           (at ["dtNascimento"] string)
                           (at ["senha"] string)
                           (at ["sexo"] string)


decodeMensagem : Decoder Mensagem
decodeMensagem = map3 Mensagem  (at ["autenticacao"] string)
                                (at ["idcadastro"] int)
                                (at ["resp"] string)

decodeRetorno : Decoder Retorno
decodeRetorno = map2 Retorno (at ["codigo"] int)
                             (at ["mensagem"] decodeMensagem)
                            


getLogin : String -> String -> Cmd Message
getLogin login senha =
    let
        url = ("https://haskelleta-romefeller.c9users.io/cadastro/busca/"++login++"/"++senha++"/login")
    in
        send Response <| Http.get url decodeRetorno

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        Login x ->
            ({model | login = x}, Cmd.none)

        Senha x ->
            ({model | senha = SHA.sha256(x)}, Cmd.none)

        Submit ->
            (model, getLogin model.login model.senha)

        Response x ->
            case x of
                Err y -> ({ model | error = (httpErrorString y) }, Cmd.none)
                Ok  y -> ({ model | ret = y }, Cmd.none)

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

view : Model -> Html Message
view model =
    section [class "center-align form-margin"]
    [
      div [class "elm-form col s12 m6 l6"]
      [
        div [class "input-field"] --NOME
        [
          input [type_ "text", required True, class "validate", onInput Login] []
          ,label [class "active"] [text "Name"]
        ]
        ,div [class "input-field"] --SENHA
        [
          input [type_ "password", required True,class "validate", onInput Senha] []
          ,label [class "active"] [text "Password"]
        ]
        ,button [class "btn waves-effect green", id "btnEnviar", onClick Submit] [text "Login"]
      ]
      , div [] [text model.error]
    ]



main =
  program
    {
     init = (init, Cmd.none)
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
    }
