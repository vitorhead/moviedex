module ConsultaFilme exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)


type Message =
      SubmitConsultaFilme
    | ResponseConsultaFilme (Result Http.Error List(ConsultaFilme))
    
type alias Model = 
    {
     resp : ConsultaFilme
    ,error : String
    ,idAPI : Int
    }

type alias ConsultaFilme =
    {
     filmesID : Int
    }     


init : Model
init = 
    Model (ConsultaFilme 0) "" 0

decodeConsultaFilme : Decoder ConsultaFilme
decodeConsultaFilme = Json.Decode.map ConsultaFilme (at ["id"] int)


-- FUNÇÃO PRA FAZER O GET NA TABELA FILMES E VER SE EXISTE NO BANCO
getConsultaFilmes : Int -> Cmd Message
getConsultaFilmes idAPI = 
    let
        url = ("https://haskelleta-romefeller.c9users.io/filmes/consultaFilme/"++ toString idAPI)
    in
        Http.send ResponseConsultaFilme <| Http.get url decodeConsultaFilme
        
        
update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        SubmitConsultaFilme ->
            (model, getConsultaFilmes model.idAPI)

        ResponseConsultaFilme x ->
            case x of
                Err y -> ({ model | error = toString y}, Cmd.none)
                Ok  y -> ({ model | resp = y}, Cmd.none)
                

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