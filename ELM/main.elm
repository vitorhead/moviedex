module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (send, get)
import Json.Decode as Decode exposing (..)
import Login as ModuloLogin
import Cadastro as ModuloCadastro
import BuscaFilme as ModuloBuscaFilme
import MeusFilmes as ModuloMeusFilmes

type Pagina = Cadastro
            | Login
            | BuscaFilme
            | MeusFilmes
            | Root

type Click =  BuscaClick
            | MeusFilmesClick
            | Nada

type Message =
      PgCadastro ModuloCadastro.Message
    | PgLogin ModuloLogin.Message
    | PgBuscaFilme ModuloBuscaFilme.Message
    | PgMeusFilmes ModuloMeusFilmes.Message
    | Mudar Pagina
    | SubmitAutenticacao Click
    | ResponseAutenticacao (Result Http.Error Bool)
    | ResponseUpcoming (Result Http.Error (List(ModuloBuscaFilme.FilmeResult)))

type alias Model =
    {
     login      : ModuloLogin.Model
    ,cadastro   : ModuloCadastro.Model
    ,buscaFilme : ModuloBuscaFilme.Model
    -- ,meusFilmes : List(MeusFilmes)
    ,meusFilmes : ModuloMeusFilmes.Model
    ,upcoming   : List(ModuloBuscaFilme.FilmeResult)
    ,janela     : Pagina
    ,acao       : Click
    }

init : (Model, Cmd Message)
init = ({login = ModuloLogin.Model "" "" "" (ModuloLogin.Retorno 0 (ModuloLogin.Mensagem "" 0 "")),
         cadastro = ModuloCadastro.Model "" "" "" "" "" (ModuloCadastro.Retorno 0 0) "",
         buscaFilme = ModuloBuscaFilme.init,
         meusFilmes = ModuloMeusFilmes.init,
         upcoming = [],
         janela = Root,
         acao = Nada}, Cmd.batch [getUpcoming])


getValidaAutenticacao : String -> Cmd Message
getValidaAutenticacao auth =
    let
        url = ("https://haskelleta-romefeller.c9users.io/cadastro/autenticacao/"++auth)
    in
        Http.send ResponseAutenticacao <| Http.get url (at ["resp"] Decode.bool)

getUpcoming : Cmd Message
getUpcoming =
  let
    url = "https://api.themoviedb.org/3/movie/upcoming?api_key=3a97c7968533c6effacc04e1449450b1&language=pt-BR&page=1"
  in
    Http.send ResponseUpcoming <| Http.get url (at ["results"] <| Decode.list ModuloBuscaFilme.decodeFilmeResult)


update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        Mudar p -> ({model | janela = p}, Cmd.none)

        PgLogin p ->
          let
            updt = ModuloLogin.update p model.login
          in
            ({model | login = Tuple.first updt}, Cmd.map PgLogin <| Tuple.second updt)

        PgCadastro p ->
          let
            updt = ModuloCadastro.update p model.cadastro
          in
            ({model | cadastro = Tuple.first updt}, Cmd.map PgCadastro <| Tuple.second updt)


        PgBuscaFilme p ->
          let
            updt = ModuloBuscaFilme.update p model.buscaFilme
            --pegando model antigo
            oldModelBuscaFilme = Tuple.first updt
            --passando idCadastro do model do main pro model do buscaFilme
            newBF = {oldModelBuscaFilme | idCadLogado = model.login.ret.mensagem.idcadastro}
          in
            ({model | buscaFilme = newBF}, Cmd.map PgBuscaFilme <| Tuple.second updt)


        PgMeusFilmes p ->
          let
            updt = ModuloMeusFilmes.update p model.meusFilmes
            --pegando model antigo
            oldMeusFilmes = Tuple.first updt
            --passando idCadastro do model do main pro model do buscaFilme
            newMeusFilmes = {oldMeusFilmes | idCadLogado = model.login.ret.mensagem.idcadastro}
          in
            ({model | meusFilmes = newMeusFilmes}, Cmd.map PgMeusFilmes <| Tuple.second updt)

        SubmitAutenticacao clicado ->
          ({model | acao = clicado}, getValidaAutenticacao <| String.filter (\x -> x /= '"') model.login.ret.mensagem.autenticacao)

        ResponseAutenticacao resp ->
          case resp of
            Err y -> ({model | janela = Root}, Cmd.none)
            Ok y ->
              case y of
                True ->
                      let
                        clicado =
                          case model.acao of
                            BuscaClick -> BuscaFilme

                            MeusFilmesClick -> MeusFilmes

                            Nada -> Root
                      in
                        ({model | janela = clicado}, Cmd.none)
                False ->
                        ({model | janela = Root}, Cmd.none)

        ResponseUpcoming resp ->
          case resp of
            Err y -> ({model | upcoming = []}, Cmd.none)
            Ok y -> ({model | upcoming = y}, Cmd.none)


viewMainPage : Model -> Html Message
viewMainPage model =
  let
    montaUpcoming : ModuloBuscaFilme.FilmeResult -> Html Message
    montaUpcoming mf =
      let
         poster = case mf.poster_path of
                    Nothing -> "--"
                    Just x -> x
      in
                li []
                [
                  div [class "poster-filme"]
                  [
                    img [src ("http://image.tmdb.org/t/p/w342/"++poster)] []
                  ]
                ]
  in

       div [class "row"]
        [
          div [class "sidebar col s12 m4 l3"]
          [
            div [class "lateral-principal"]
            [
              ul []
              [
                li [] [text "NOME CHAMPS"]
                ,li []
                [
                  a [class "btn green", onClick <| SubmitAutenticacao BuscaClick] [text "Buscar Filmes"]
                ]
                ,li []
                [
                  button [class "btn red"] [text "Deslogar"]
                ]
              ]
            ]
          ]

          ,div [class "col s12 m8 l9"]
          [
            h1 [onClick <| SubmitAutenticacao MeusFilmesClick] [text "MEUS FILMES"]
          ]

          ,section []
          [
            h1 [] [text <|"Lançamentos: "]
            ,ul []
            [
              div [class "lista wrap"] (List.map montaUpcoming model.upcoming)
            ]
          ]
        ]


viewRoot : Html Message
viewRoot =
  div []
  [
     section [class "apresentacao"]
      [
        h2 [] [text "MovieDex"]
        ,p [] [text "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Labore, nihil."]
        ,div []
        [
          a [onClick (Mudar Cadastro) ,class "btn green"] [text "Cadastro"]
          ,a [class "btn", onClick (Mudar Login)] [text "Login"]
        ]
      ]
    ,section [id "quem-somos", class "container"]
      [
        div [class "row center-align"]
        [
          a [href "https://github.com/guimon23", class "col s12 m6"]
           [
            img [src "", class "responsive-img circle", alt "Github Ramon"] []
          ]
          ,p [class "col s12 m6"] [text "Ramon Gaspar, XX anos, músico e positividade"]
        ]
        ,div [class "row center-align"]
      [
        a [href "https://github.com/vitorhead", class "col s12 m6"]
        [
          img [src "../static/images/vitor.jpeg", class "responsive-img circle", alt "Github Vitor"] []
        ]
        ,p [class "col s12 m6"] [text "Vitor Stipanich, XX anos, nos compiuters e jogos online"]
      ]
        ,div [class "row center-align"]
      [
        a [href "https://github.com/castylho", class "col s12 m6"]
        [
          img [src "../static/images/Yohann.jpg", class "responsive-img circle", alt "Github Yohann"] []
        ]
        ,p [class "col s12 m6"] [text "Yohann Castilho, 23 anos, avido jogador de Skyrom e Fifa"]
      ]
      ]
  ]


view : Model -> Html Message
view model =
    let
      deslogado : Html Message
      deslogado =
        case model.janela of
            Login -> Html.map PgLogin <| ModuloLogin.view model.login

            Cadastro -> Html.map PgCadastro <| ModuloCadastro.view model.cadastro

            Root -> viewRoot

            BuscaFilme -> Html.map PgBuscaFilme <| ModuloBuscaFilme.view model.buscaFilme

            MeusFilmes -> Html.map PgMeusFilmes <| ModuloMeusFilmes.view model.meusFilmes


      logado : Html Message
      logado =
          case model.janela of
            Login -> viewMainPage model

            Cadastro -> viewMainPage model

            Root -> viewMainPage model

            BuscaFilme -> Html.map PgBuscaFilme <| ModuloBuscaFilme.view model.buscaFilme

            MeusFilmes -> Html.map PgMeusFilmes <| ModuloMeusFilmes.view model.meusFilmes


    in
     if model.login.ret.mensagem.autenticacao == "" then
      deslogado
     else
      logado


main = program
    { init = init
    , view = view
    , update = update
    , subscriptions =  \_ -> Sub.none
    }
