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
    div [] []


main =
  program
    {
     init = (init, Cmd.none)
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
    }