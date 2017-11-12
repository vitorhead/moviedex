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
             
main =
  program 
    { 
     init = (init, Cmd.none)
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
    }       