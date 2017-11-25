module Cadastro exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (..)
import Sha256 as SHA exposing (sha256)


httpErrorRetorno : Error -> String
httpErrorRetorno error =
    case error of
        BadUrl text -> ("[ERRO HTTP] Bad Url: " ++ text)

        Timeout -> "[ERRO HTTP] Timeout"

        NetworkError -> "[ERRO HTTP] Network Error"

        BadStatus response -> ("[ERRO HTTP] Status: " ++ toString response.status.code)

        BadPayload message response -> ("[ERRO HTTP] Payload incorreto: "++
             toString message++"("++toString response.status.code++")")

type alias Retorno =
    {
     mensagem : Int --id do cadastro
    ,codigo : Int
    }

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
     nome         : String
    ,dtNascimento : String
    ,sexo         : String
    ,email        : String
    ,senha        : String
    ,retorno      : Retorno
    ,error        : String
    }

type Message =
      Nome String
    | DtNascimento String
    -- | SetDatePicker DatePicker.Msg
    | Sexo String
    | Email String
    | Senha String
    | Submit
    | Response (Result Http.Error Retorno)

init : Model
init =
    let
        retornoIni = Retorno 0 0
    in
        (Model "" "" "" "" "" retornoIni "")

urlPOST : String
urlPOST = "https://haskelleta-romefeller.c9users.io/cadastro/inserir"

decodeInserirRetorno : Decoder Retorno
decodeInserirRetorno = map2 Retorno (at ["mensagem"] Decode.int)
                                    (at ["codigo"] Decode.int)

encodeCad : Model -> Encode.Value
encodeCad cad =
    let
        lstCad =
        [
         ("email", Encode.string <| cad.email)
        ,("senha", Encode.string <| cad.senha)
        ,("nome", Encode.string <| cad.nome)
        ,("dtNascimento", Encode.string <| cad.dtNascimento)
        ,("sexo", Encode.string <| cad.sexo)
        ]
    in
        Encode.object <| lstCad


postCadastro : Model -> String -> Cmd Message
postCadastro cad url =
    let
        requestBody = Http.jsonBody <| encodeCad cad
    in
        Http.send Response <|
                            Http.post url requestBody decodeInserirRetorno --o retorno do POST é um ID só

update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        Nome x ->
            ({model | nome = x}, Cmd.none)

        Email x ->
            ({model | email = x}, Cmd.none)

        Senha x ->
            ({model | senha = SHA.sha256(x)}, Cmd.none)

        DtNascimento x ->
            ({model | dtNascimento = x}, Cmd.none)

        Sexo x ->
            ({model | sexo = x}, Cmd.none)

        Submit ->
            (model, postCadastro model urlPOST)

        Response x ->
            case x of
                Err y -> ({ model | error = (httpErrorRetorno y) }, Cmd.none)
                Ok  y -> ({ model | retorno = y }, Cmd.none)



-- formataData : String -> String
-- formataData dt = String.split "/"

view : Model -> Html Message
view model =
  section [class "center-align form-margin"]
  [
      div [class "elm-form col s12 m6 l6"]
      [
        div [class "input-field"] --NOME
        [
          input [type_ "text", required True, class "validate", onInput Nome] []
          ,label [class "active"] [text "Name"]
        ]
        ,div [class "input-field"] --EMAIL
        [
          input [type_ "email", id "email" , required True,class "validate", onInput Email] []
          ,label [class "active"] [text "Email"]
        ]
        ,div [class "input-field"] --SENHA
        [
          input [type_ "password", required True,class "validate", onInput Senha] []
          ,label [class "active"] [text "Password"]
        ]
        ,div [class "input-field"] --DATA NASCIMENTO
        [
        input [placeholder "dd/mm/aaaa"] []
        ,label [class "active"] [text "Data de nascimento"]
        ]
         --SEXO SELECT/RADIO
        ,button [type_ "submit", class "btn waves-effect green center-align"] [text "Cadastrar"]
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
