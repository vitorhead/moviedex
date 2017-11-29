module BuscaFilme exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


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

formatFilmeResult : FilmeResult -> Html Message
formatFilmeResult fr =
    let
        linhaFoto = case fr.poster_path of
                        Nothing -> "---"
                        Just x  -> urlFoto++x
        
    in
    div [class "info-filme"]
    [
      img [class "responsive-img",src linhaFoto] []
     ,div []
     [
        button [class "btn btn-filme", onClick <| SubmitConsultaFilme fr.id] 
        [ 
         i [class "material-icons small"] [text "add"]
        ]
       ,button [class "btn btn-filme"]
        [
         i [class "material-icons small"] [text "dehaze"]
        ]
    --   p [] [text <| "ID: "++(toString fr.id)]
    --  ,p [] [text <| "Titulo: "++fr.title]
    --  ,p [] [text <| "Nota:"++(toString fr.vote_average)]
    --  ,p [] [text <| "Data de lancamento:"++fr.release_date]
    --  ,p [] [text <| "Sinopse: "++fr.overview]
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
                h5 [] [text "DADOS DA BUSCA: "]
                ,br [] []
                ,div [class "grid"] (List.map formatFilmeResult f.results)
            ]
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

type alias FilmesCad = 
    {
     idFilme : Int
    ,idCadastro : Int
    ,assistido : Bool
    ,favorito : Bool
    }

type Message =
      NomeFilme String
    | SubmitBusca
    | ResponseBusca (Result Http.Error Filme)
    | SubmitConsultaFilme Int
    | ResponseConsultaFilme (Result Http.Error Int)
    | SubmitInsereFilmesCad
    | ResponseInsereFilmesCad (Result Http.Error Int)


encodeFilmesCad : FilmesCad -> Encode.Value
encodeFilmesCad fc =
    let
        lstFC =
        [
         ("idFilme", Encode.int <| fc.idFilme)
        ,("idCadastro", Encode.int <| fc.idCadastro)
        ,("assistido", Encode.bool <| fc.assistido)
        ,("favorito", Encode.bool <| fc.favorito)
        ]
    in
        Encode.object <| lstFC

--POST pra inserir na filmescad
postInsereFilmesCad : FilmesCad -> Cmd Message
postInsereFilmesCad fc =
    let
        url = "https://haskelleta-romefeller.c9users.io/filmescad/inserir"
        requestBody = Http.jsonBody <| encodeFilmesCad fc
    in
        Http.send ResponseInsereFilmesCad <|
                            Http.post url requestBody Decode.int --o retorno do POST é um ID só

type alias Model =
    {
     nomeFilme          : String
    ,error              : String
    ,resultadoBusca     : Filme
    ,filmesIdConsulta   : Int
    ,idCadLogado        : Int
    }

init : Model
init =
    let
        initFilme = Filme 0 0 0 []   
    in
        Model "" "" initFilme 0 0

--DECODER DA API DE FILMES
decodeFilmeResult : Decoder FilmeResult
decodeFilmeResult = map6 FilmeResult (at ["id"] Decode.int)
                                     (at ["title"] Decode.string)
                                     (at ["vote_average"] Decode.float)
                                (maybe((at ["poster_path"] Decode.string)))
                                     (at ["overview"] Decode.string)
                                     (at ["release_date"] Decode.string)

--DECODER DA API DE FILMES
decodeFilme : Decoder Filme
decodeFilme = map4 Filme (at ["page"] Decode.int)
                         (at ["total_results"] Decode.int)
                         (at ["total_pages"] Decode.int)
                         (at ["results"] (Decode.list decodeFilmeResult))
                         
--GET NA API DE FILMES
getFilme : String -> Cmd Message
getFilme nomefilme =
    let
        url = ("https://api.themoviedb.org/3/search/movie?api_key=3a97c7968533c6effacc04e1449450b1&language=en-US&query="++nomefilme++"&page=1&include_adult=false")
    in
        send ResponseBusca <| Http.get url decodeFilme


-- DECODE PRO GET DO CONSULTA FILMES
decodeConsultaFilme : Decoder Int
decodeConsultaFilme = (at ["id"] Decode.int)

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
        NomeFilme x ->
            ({model | nomeFilme = formatarNome x}, Cmd.none)
        

        SubmitBusca ->
            ({model | resultadoBusca = (Filme 0 0 0 []),
                      error = ""}, getFilme model.nomeFilme)
                      
        ResponseBusca x ->
            case x of
                Err y -> ({ model | error = (httpErrorString y) }, Cmd.none)
                Ok  y -> ({ model | resultadoBusca = y }, Cmd.none)
                
        SubmitConsultaFilme idAPI ->
            (model, getConsultaFilmes idAPI)
            
        ResponseConsultaFilme x ->
            let
              cadInserir = FilmesCad model.filmesIdConsulta model.idCadLogado False False
              cmdInsereFilmesCad = postInsereFilmesCad cadInserir
            in
            case x of
                Err y -> ({model | error = (httpErrorString y)}, Cmd.none)
                Ok  y -> ({model | filmesIdConsulta = y}, cmdInsereFilmesCad)
        
        SubmitInsereFilmesCad ->
            (model, Cmd.none)
        
        ResponseInsereFilmesCad x ->
            (model, Cmd.none)

view : Model -> Html Message
view model =
    div [class "divGeral"]
    [
      div [class "container center-align"]
      [
        div [class "input-field inline"]
        [
         input [type_ "text", class "validate", placeholder "nome do filme que deseja buscar", required True, onInput NomeFilme] []
        ,button [class "btn green waves-effect", onClick SubmitBusca] [text "BUSCAR!"]
        ]
      ]
      ,div [class "center-align"]
      [
         div [style [("color", "red")]] [text <| toString model.error]
        ,div [] [viewFilme model.resultadoBusca]
        ,label [] [text <| "id do filme (nossa base): "++toString model.filmesIdConsulta]
        ,label [] [text <| "cadastro logado: "++toString model.idCadLogado]
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
