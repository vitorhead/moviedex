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
             
main =
  program 
    { 
     init = (init, Cmd.none)
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
    }       