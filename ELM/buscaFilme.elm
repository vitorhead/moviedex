module BuscaFilme exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)

{-
    API utilizada: The Movie DB API
    https://developers.themoviedb.org/3 
    
    moviedexHaskell@gmail.com
    !Moviedex123
    
    haskellmovie
    !Moviedex123
    
    API KEY: 3a97c7968533c6effacc04e1449450b1
    Cadastrei uns dados fakes rs
    
    PARAMETROS DE CONFIGURAÇÃO DA API:
    https://api.themoviedb.org/3/configuration?api_key=3a97c7968533c6effacc04e1449450b1
    A implementação recomendada é cachear isso no início do programa e ir usando.
    No momento deixei fixo 
    http://image.tmdb.org/t/p/w154/<<posterpath>>
    
    posterpath tamanhos:
    	"w92"
        "w154"
        "w185"
        "w342"
        "w500"
        "w780"
        "original"
-}

{-
map14 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> value) ->
 Decoder a ->
 Decoder b ->
 Decoder c ->
 Decoder d ->
 Decoder e ->
 Decoder f ->
 Decoder g ->
 Decoder h ->
 Decoder i ->
 Decoder j ->
 Decoder k ->
 Decoder l ->
 Decoder m ->
 Decoder n ->
 Decoder value

map14 f a b c d e f g h i j k l m n =
 f head(a) 
   head(b) 
   head(c) 
   head(d) 
   head(e) 
   head(f) 
   head(g) 
   head(h) 
   head(i) 
   head(j) 
   head(k) 
   head(l) 
   head(m) 
   head(n) :: map14 f a b c d e f g h i j k l m n
   
   
   type alias FilmeResult1 =
    {
     vote_count         : Int
    ,id                 : Int
    ,video              : Bool
    ,vote_average       : Float
    ,title              : String
    ,popularity         : Float
    ,poster_path        : String
    ,original_language  : String
    ,original_title     : String
    ,genre_ids          : List(Int)
    ,backdrop_path      : String
    ,adult              : Bool
    ,overview           : String
    ,release_date       : String
    }
-}
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
             
urlFoto : String
urlFoto = "http://image.tmdb.org/t/p/w154/"

--%20 = espaço na query da URL
-- ex: Hello World
-- o Split vai dividir em uma lista: ["Hello", "World"]
--   => split " " Hello World = ["Hello", "World"]
-- o Join vai unir a lista com um delimitador: "Hello%20World" 
--   => join "/" ["vitu", "home", "Download"]  = vitu/home/Downloads
formatarNome : String -> String
formatarNome nome = String.split " " nome |> String.join "%20"


view : Model -> Html Message
view model = 
    div [class "divGeral"]
    [ 
         input [type_ "text", placeholder "nome do filme que deseja buscar", required True, onInput NomeFilme] []
        ,button [onClick Submit] [text "BUSCAR!"]
        ,div [style [("color", "red")]] [text <| toString model.error]
        --,div [] [text <| toString model.nomeFilme] 
        ,div [] [viewFilme model.resultadoBusca]
    ]
    
    
main =
  program 
    { 
     init = (init, Cmd.none)
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
    }        