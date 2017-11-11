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

styleLinha : Html.Attribute Message
styleLinha = style [
                    ("background", "lightgray"),
                    ("margin-bottom", "1px")
                   ]

styleListaLinhas : Html.Attribute Message
styleListaLinhas = style [
                            ("position", "relative"),
                            ("right", "200"),
                            ("top", "200"),
                            ("display", "inline")
                         ]
formatFilmeResult : FilmeResult -> Html Message
formatFilmeResult fr = 
    let
        linhaFoto = case fr.poster_path of
                        Nothing -> "---"
                        Just x  -> urlFoto++x
    in
    div [] 
    [
      img [src linhaFoto, style [("display", "inline")]] []
     ,ul [styleListaLinhas]
     [
      li [styleLinha] [text <| "ID: "++(toString fr.id)]
     ,li [styleLinha] [text <| "Titulo: "++fr.title]
     ,li [styleLinha] [text <| "Nota:"++(toString fr.vote_average)]
     ,li [styleLinha] [text <| "Data de lancamento:"++fr.release_date]
     ,li [styleLinha] [text <| "Sinopse: "++fr.overview]
     ]
    ]

--teste pra jogar no html
viewFilme : Filme -> Html Message
viewFilme f =
    div [] 
    [
     label [] [text <| "pagina "++toString f.page ++ " - " ++ "total :"++toString f.total_results ++ " - " ++ "total de paginas: "++toString f.total_pages]
    ,br [] []
    ,div [] [
                label [] [text "DADOS DA BUSCA: "]
                ,br [] []
                ,div [style[("position", "absolute")]] (List.map formatFilmeResult f.results)
            ]
    ]

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

type alias FilmeResult =
    {
     id                  : Int
    ,title               : String
    ,vote_average        : Float
    ,poster_path         : Maybe String
    ,overview            : String
    ,release_date        : String
    }

type alias Filme =
    {
     page           : Int
    ,total_results  : Int
    ,total_pages    : Int
    ,results        : List(FilmeResult)
    }


type Message = 
      NomeFilme String
    | Submit
    | Response (Result Http.Error Filme)

type alias Model = 
    {
     nomeFilme          : String
    ,error              : String
    ,resultadoBusca     : Filme
    }

init : Model
init = 
    let 
        initFilme = Filme 0 0 0 []
    in
        Model "" "" initFilme

decodeFilmeResult : Decoder FilmeResult
decodeFilmeResult = map6 FilmeResult (at ["id"] int)
                                     (at ["title"] string)
                                     (at ["vote_average"] float)
                                (maybe((at ["poster_path"] string)))
                                     (at ["overview"] string)
                                     (at ["release_date"] string)

decodeFilme : Decoder Filme
decodeFilme = map4 Filme (at ["page"] int)
                         (at ["total_results"] int)
                         (at ["total_pages"] int)
                         (at ["results"] (Json.Decode.list decodeFilmeResult))
                         
getFilme : String -> Cmd Message
getFilme nomefilme =
    let 
        url = ("https://api.themoviedb.org/3/search/movie?api_key=3a97c7968533c6effacc04e1449450b1&language=en-US&query="++nomefilme++"&page=1&include_adult=false")
    in
        send Response <| Http.get url decodeFilme
        
        
update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        NomeFilme x ->
            ({model | nomeFilme = formatarNome x}, Cmd.none)
        
        Submit ->
            ({model | resultadoBusca = (Filme 0 0 0 []), 
                      error = ""}, getFilme model.nomeFilme)
            
        Response x ->
            case x of
                Err y -> ({ model | error = (httpErrorString y) }, Cmd.none)
                Ok  y -> ({ model | resultadoBusca = y }, Cmd.none)

    
main =
  program 
    { 
     init = (init, Cmd.none)
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
    }        